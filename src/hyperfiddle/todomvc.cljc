(ns hyperfiddle.todomvc
  (:require [clojure.edn :refer [read-string]]
            [datascript.core :as d]
            [missionary.core :as m]
            [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros [hyperfiddle.todomvc :refer [db basis-t todo-list transact!']])))

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

(p/def db)
(p/def basis-t)

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
                          (do
                            (println ::fsm-v)
                            #_(dom/pick >v')                ; detect ack and discard
                            (m/?> >v')
                            (println ::fsm-v')
                            (recur)))))))

(defn clear-input! [el v] (dom/set-property! el "value" "") v)

#?(:cljs (defn log [x] (doto x js/console.log)))

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
                  (fsm (m/eduction (drop 1) #'basis-t)))))
        (dom/div
          (apply concat
                 (p/for [id ~@(d/q '[:find [?e ...] :in $ :where [?e :task/status]] db)]
                   (let [status ~@(d/q '[:find ?s . :in $ ?e :where [?e :task/status ?s]] db id)
                         description ~@(d/q '[:find ?s . :in $ ?e :where [?e :task/description ?s]] db id)]
                     (dom/div
                       (concat
                         (dom/input
                           (dom/attribute "type" "checkbox")
                           (dom/set-checked! dom/parent (#{:done} status))
                           ~(->> (dom/events dom/parent dom/input-event)
                                 (m/eduction
                                   (map dom/event-target)
                                   (map dom/get-checked)
                                   (map {false :active, true :done})
                                   (map (partial task-status id)))
                                 (fsm (m/eduction (drop 1) #'basis-t))))
                         (dom/span (dom/text (str id "-" description)))))))))
        (dom/div
          (dom/span
            (dom/text (str ~@(count (d/q '[:find [?e ...] :in $ ?status
                                           :where [?e :task/status ?status]]
                                         db :active)) " items left")))))))

(def !conn #?(:clj (d/create-conn {})))
(def !stage #?(:cljs (atom nil)))                            ; we choose stage on client

(p/defn transact!' [stage]
  (fsm
    #'nil
    ~@(do (d/transact! !conn stage) nil)))

(def auto-inc-2 (let [!x (atom 0)] (fn [stage]
                                     (println ::auto-inc stage @!x)
                                     (swap! !x inc))))

(p/def app
  #'(let [stage ~(m/eduction (dedupe) (m/watch !stage))]
      ~@(let [tx-report (d/with ~(m/watch !conn) stage)]
          (println ::tx-report tx-report)
          (binding [db (:db-after tx-report)]
            (let [current-tx (-> tx-report :tempids :db/current-tx)]
              ~@(binding [basis-t (auto-inc-2 stage)]
                  (let [tx-data (seq ~todo-list)]
                    (when tx-data
                      (js/console.log ::tx-data tx-data)
                      (swap! !stage concat tx-data)))))))

      (dom/text "transact!")
      (when-some [click-event (dom/button
                                (dom/text "transact!")
                                (dom/set-attribute! dom/parent "type" "button")
                                ~(fsm #'nil #_(m/eduction (drop 1) (m/watch !conn))
                                      (dom/events dom/parent "click")))]
        (println ::transact! click-event)
        (p/$ transact!' stage)
        #_~@(do (d/transact! !conn stage) nil)
        (reset! !stage []))

      (dom/text "staging area")
      (if-some [tx-data (dom/input
                          (dom/attribute "type" "text")
                          (dom/property "value" (pr-str stage))
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