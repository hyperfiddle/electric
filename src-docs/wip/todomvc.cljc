(ns user.todomvc
  (:require [clojure.edn :as edn]
            [datascript.core :as d]
            [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z])
  #?(:cljs (:require-macros user.todomvc)))

(def auto-inc (partial swap! (atom 0) inc))

(defn task-create [description]
  [{:db/id            (str "dustin-" (auto-inc))
    :task/description description
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

(defn clear-input! [el v] (dom/set-property! el "value" "") v)

#?(:cljs (defn log [x] (doto x js/console.log)))

(p/defn Todo-list [db basis-t]
    (dom/div
      (concat
        (dom/div
          (dom/text "TodoMVC")
          (dom/input
            (dom/attribute "type" "text")
            (z/impulse basis-t
              (->> (dom/events dom/parent "keyup")
                (m/eduction
                  (filter (comp #{dom/keycode-enter} dom/keycode))
                  (map (comp task-create dom/target-value))
                  (map (partial clear-input! dom/parent)))))))
        (dom/div
          (apply concat
            (dom/for [id ~@(d/q '[:find [?e ...] :in $ :where [?e :task/status]] db)]
              (let [status ~@(d/q '[:find ?s . :in $ ?e :where [?e :task/status ?s]] db id)
                    description ~@(d/q '[:find ?s . :in $ ?e :where [?e :task/description ?s]] db id)]
                (dom/div
                  (concat
                    (dom/input
                      (dom/attribute "type" "checkbox")
                      (dom/set-checked! dom/parent (#{:done} status))
                      (z/impulse basis-t
                        (->> (dom/events dom/parent dom/input-event)
                          (m/eduction
                            (map dom/event-target)
                            (map dom/get-checked)
                            (map {false :active, true :done})
                            (map (partial task-status id))))))
                    (dom/span (dom/text (str id "-" description)))))))))
        (dom/div
          (dom/span
            (dom/text (str ~@(count (d/q '[:find [?e ...] :in $ ?status
                                           :where [?e :task/status ?status]]
                                      db :active)) " items left")))))))

(def !conn (d/create-conn {}))

(defn transact [tx-data]
  (prn :tx-data tx-data)
  (d/transact! !conn tx-data) 1)

(p/defn app []
  (let [!t      (atom 0)
        basis-t (p/Watch. !t)]
    (prn :basis-t basis-t)
    ~@(let [db (p/Watch. !conn)]
        ~@(if-some [tx-data (seq (Todo-list. db basis-t))]
            (swap! !t + ~@(transact tx-data))
            (prn :idle)))))

(def !stage #?(:cljs (atom nil)))                            ; we choose stage on client

(p/defn Transact! [stage]
  (z/fsm nil
    (p/fn [] nil)
    ~@(do (d/transact! !conn stage) nil)))

(def auto-inc-2 (let [!x (atom 0)] (fn [stage]
                                     (println ::auto-inc stage @!x)
                                     (swap! !x inc))))

(p/defn app-staging-fixme [db basis-t]
    (let [stage ~(m/eduction (dedupe) (m/watch !stage))]
      ~@(let [tx-report (d/with ~(m/watch !conn) stage)]
          (println ::tx-report tx-report)
          (let [db (:db-after tx-report)
                current-tx (-> tx-report :tempids :db/current-tx)]
            ~@(let [basis-t (auto-inc-2 stage)]
                (let [tx-data (seq (Todo-list. db basis-t))]
                  (when tx-data
                    (js/console.log ::tx-data tx-data)
                    (swap! !stage concat tx-data))))))

      (dom/text "transact!")
      (when-some [click-event (dom/button
                                (dom/text "transact!")
                                (dom/set-attribute! dom/parent "type" "button")
                                ~(z/fsm nil (fn [] nil) #_(m/eduction (drop 1) (m/watch !conn))
                                      (dom/events dom/parent "click")))]
        (println ::transact! click-event)
        (Transact!. stage)
        #_~@(do (d/transact! !conn stage) nil)
        (reset! !stage []))

      (dom/text "staging area")
      (if-some [tx-data (dom/input
                          (dom/attribute "type" "text")
                          (dom/property "value" (pr-str stage))
                          ~(z/fsm nil (fn [] nil) (->> (dom/events dom/parent "input")
                                           (m/eduction
                                             (map dom/target-value)
                                             (dedupe)))))]
        (do
          (js/console.log ::stage tx-data)
          (reset! !stage (edn/read-string tx-data)))
        (do (js/console.log ::stage ::idle)
            nil))))

; button
; to transact
; clear staging area
; get new time basis
; Does d/with and d/transact return the same time basis for the same log value
; touch query with latest time basis
