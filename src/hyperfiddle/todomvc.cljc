(ns hyperfiddle.todomvc
  (:require [datascript.core :as d]
            [missionary.core :as m]
            [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros [hyperfiddle.todomvc :refer [db report transact todo-list]])))

(defn task-create [description]
  [{:task/description description
    :task/status      :active}])

(defn task-status [id status]
  [{:db/id id
    :task/status status}])

(defn task-remove [id]
  ;; TODO
  )

(def !conn (d/create-conn {}))
(p/def db ~(m/watch !conn))

(comment
  (d/q '[:find ?s . :in $ ?e :where [?e :task/status ?s]] @!conn 1)

  (d/transact !conn (task-create "buy milk"))


  )

(def !ack (atom {}))
(p/def report ~(m/watch !ack))

(defn fsm [>report]
  (fn [>tx]
    (m/ap
      (loop []
        (m/amb> nil
          (let [tx (dom/pick >tx)]
            (m/amb> tx
              (do (dom/pick >report)
                  (recur)))))))))

(p/def transact (fsm (m/eduction (remove nil?) #'report)))

(p/def todo-list
  #'(dom/div
      (concat
        (dom/div
          (dom/input
            (dom/set-attribute! dom/parent "type" "text")
            ~(->> (dom/events dom/parent dom/keydown-event)
               (m/eduction
                 (filter (comp #{dom/keycode-enter} dom/keycode))
                 (map dom/event-target)
                 (map dom/get-value)
                 (map task-create))
               (transact))))
        (dom/div
          (apply concat
            (p/for [id ~@(d/q '[:find [?e ...] :in $ :where [?e :task/status]] db)]
              (let [status ~@(d/q '[:find ?s . :in $ ?e :where [?e :task/status ?s]] db id)
                    description ~@(d/q '[:find ?s . :in $ ?e :where [?e :task/description ?s]] db id)]
                (dom/div
                  (concat
                    (dom/input
                      (dom/set-attribute! dom/parent "type" "checkbox")
                      (dom/set-checked! dom/parent (#{:done} status))
                      ~(->> (dom/events dom/parent dom/input-event)
                         (m/eduction
                           (map dom/event-target)
                           (map dom/get-checked)
                           (map {false :active, true :done})
                           (map (partial task-status id)))
                         (transact)))
                    (dom/span
                      (dom/set-text-content! dom/parent description))
                    #_
                    (dom/input
                      (dom/set-attribute! dom/parent "type" "button")
                      (dom/set-attribute! dom/parent "value" "remove")
                      ~(->> (dom/events dom/parent dom/click-event)
                         (m/eduction
                           (map (constantly id))
                           (map task-remove))
                         (transact)))))))))
        (dom/div
          (dom/span
            (dom/set-text-content! dom/parent (str ~@(count (d/q '[:find [?e ...] :in $ ?status
                                                                   :where [?e :task/status ?status]]
                                                              db :active)) " items left")))))))

(p/def app
  #'(reset! !ack
      (when-some [tx-data (seq ~todo-list)]
        ~@(:tempids (d/transact! !conn tx-data)))))

(def exports (p/vars d/q d/transact! !conn))