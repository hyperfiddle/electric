(ns hyperfiddle.todomvc
  (:require [clojure.edn :refer [read-string]]
            [datascript.core :as d]
            [missionary.core :as m]
            [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros [hyperfiddle.todomvc :refer [db report todo-list]])))

(defn task-create [description]
  [{:task/description description
    :task/status      :active}])

(defn task-status [id status]
  [{:db/id id
    :task/status status}])

(defn task-remove [id]
  ;; TODO
  )

(comment
  (d/q '[:find ?s . :in $ ?e :where [?e :task/status ?s]] @!conn 1)

  (d/transact !conn (task-create "buy milk"))


  )

(p/def db)

(def !ack (atom {}))
(p/def report ~(m/watch !ack))
(p/def tx ~(m/watch !ack))




(defn fsm
  "Produce a continuous time impulse with value v which will soon be acknowledged at which point the
  impulse is cleared. the ack is a flow of anything, we ignore the value, detecting only that an
  event happened

          v -|       -------
   >v        |      |       |
        nil -|------         -----------


         any -|              -----------
   >ack       |             |
         any -|-------------
    " [>v' >v]
  (m/ap
    (loop []
      (m/amb> nil (m/amb> (dom/pick >v)
                          (do (dom/pick >v')                ; detect ack and discard
                              (recur)))))))

(defn clear-input! [el v] (dom/set-property! el "value" "") v)

(p/def todo-list
  #'(dom/div
      (concat
        (dom/div
          (dom/text "TodoMVC")
          (dom/input
            (dom/attribute "type" "text")
            ~(->> (dom/events dom/parent "keyup" #_dom/keydown-event)
                  (m/eduction
                    (filter (comp #{dom/keycode-enter} dom/keycode))
                    (map (comp task-create dom/target-value))
                    (map (partial clear-input! dom/parent)))
                  (fsm (m/eduction (drop 1) #'db)))))
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
                                 (fsm (m/eduction (drop 1) #'db))))
                         (dom/span
                           (dom/set-text-content! dom/parent description))))))))
        (dom/div
          (dom/span
            (dom/set-text-content! dom/parent (str ~@(count (d/q '[:find [?e ...] :in $ ?status
                                                                   :where [?e :task/status ?status]]
                                                                 db :active)) " items left")))))))

(def !conn (d/create-conn {}))
(def !stage #?(:cljs (atom [])))                            ; we choose stage on client

(p/def app
  #'(let [stage ~(m/watch !stage)]
      ~@(let [tx-report (d/with ~(m/watch !conn) stage)]
          (println ::tx-report tx-report)
          (binding [db (:db-after tx-report)]
            ~@(when-some [tx-data (seq ~todo-list)]
                (js/console.log ::tx-data tx-data)
                (swap! !stage concat tx-data))))

      (dom/text "transact!")
      (when-some [click-event (dom/button
                                (dom/text "transact!")
                                (dom/set-attribute! dom/parent "type" "button")
                                ~(fsm #'nil #_(m/eduction (drop 1) (m/watch !conn))
                                      (dom/events dom/parent "click")))]
        (js/console.log ::transact! click-event)
        ~@(do (d/transact! !conn stage) nil)
        (reset! !stage []))

      (dom/text "staging area")
      (if-some [tx-data (dom/input
                          (dom/attribute "type" "text")
                          (dom/attribute "value" (pr-str stage))
                          ~(fsm #'nil (->> (dom/events dom/parent "input")
                                           (m/eduction
                                             (map dom/target-value)
                                             (dedupe)))))]
        (do
          (js/console.log ::stage tx-data)
          (reset! !stage (clojure.edn/read-string tx-data)))
        (do (js/console.log ::stage ::idle)
            nil))))

; button
; to transact
; clear staging area
; get new time basis
; Does d/with and d/transact return the same time basis for the same log value
; touch query with latest time basis

(def exports (p/vars d/q d/transact! !conn println d/with))