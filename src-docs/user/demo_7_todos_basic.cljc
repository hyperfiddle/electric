(ns user.demo-7-todos-basic
  (:require clojure.edn
            [datascript.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  (:import [hyperfiddle.photon Pending]
           (missionary Cancelled))
  #?(:cljs (:require-macros user.demo-7-todos-basic)))

;;; Business logic

(def auto-inc
  "A statefull function returning 1, then 2, then 3, and so on."
  (partial swap! (atom 0) inc))

(defn task-create [description]
  {:db/id            (auto-inc)
   :task/description description
   :task/status      :active})

(defn task-status [id done?]
  {:db/id       id
   :task/status (if done? :done :active)})

(defn task-remove [id])                 ; todo

;;; Dom input getters/setters

(defn get-input-value [dom-node] (dom/oget dom-node :value))
(defn clear-input! [dom-node] (dom/oset! dom-node :value ""))

;;; Database

(def !conn #?(:clj (d/create-conn {})))

(comment ; tests
  (d/transact !conn (task-create "repl test"))

  (d/q '[:find [?e ...] :in $ :where [?e :task/status]] (d/db !conn))
  (d/q '[:find ?s . :in $ ?e :where [?e :task/status ?s]] @!conn 1)
  (d/transact !conn [{:db/id 1, :task/status :active}])
  := :active
  )

;;; Photon App

(p/defn Todo-list [db]
  (let [time-basis (:max-tx db)]        ; latest tx time, used to acknowledge a value has been saved (transacted) on server
    ~@(dom/div
        (dom/h1 (dom/text "Todo list - basic"))
        (let [{:keys [on-keychord]} (ui/input {:placeholder "Press enter to create a new item"
                                               :on-keychord
                                               [time-basis ; acknowledgement ; TODO remove from userland
                                                #{"enter"} ; key combo(s) to listen to
                                                (p/fn [js-event]
                                                  (when js-event
                                                    (let [dom-node    (dom/oget js-event :target)
                                                          description (get-input-value dom-node)]
                                                      (clear-input! dom-node)
                                                      description)))]})]
          (when on-keychord
            [::tx-statement (task-create on-keychord)]))
        (dom/div
          (p/for [id ~@(d/q '[:find [?e ...] :in $ :where [?e :task/status]] db)]
            (dom/label {:style {:display :block}}
              (let [status             ~@(:task/status (d/entity db id)) ; could be inlined, temporary hack for https://www.notion.so/hyperfiddle/Bug-Nested-p-for-with-transfers-times-out-887c3de8a0e04462b173728869c9da6a
                    {:keys [on-input]} (ui/checkbox
                                         {:checked  (case status :active false, :done true)
                                          :on-input [time-basis ; acknowledgement
                                                     (p/fn [js-event]
                                                       (when js-event
                                                         (dom/oget js-event :target :checked) ; boolean
                                                         ))]})]
                (when-not (nil? on-input) ; nil | true | false
                  [::tx-statement (task-status id on-input)]))
              (dom/span (dom/text (str ~@(:task/description (d/entity db id))))))))
        (dom/p
          (dom/text (str ~@(count (d/q '[:find [?e ...] :in $ ?status
                                         :where [?e :task/status ?status]]
                                    db :active)) " items left"))))))

(defn transact [tx-data] #?(:clj (do (prn `transact tx-data)
                                     (d/transact! !conn tx-data)
                                     nil)))

(defn commands->tx
  "Return a datomic transaction from a set of ui commands"
  [commands]
  (->> commands
    (map (fn [[name value]]
           (case name
             ::tx-statement value
             nil)))
    (remove nil?)
    (vec)))

(p/defn App []
  ~@(if-some [tx (seq (commands->tx (p/deduping (Todo-list. (p/watch !conn)))))]
      (transact tx) ; auto-transact, prints server-side
      (prn :idle)))

(def main #?(:cljs (p/client (p/main (try (binding [dom/node (dom/by-id "root")]
                                            (App.))
                                          (catch Pending _)
                                          (catch Cancelled _))))))

(comment
  (user/browser-main! `main)
  )
