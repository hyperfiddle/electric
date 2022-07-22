(ns user.replit
  (:require [datascript.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros user.replit)))

(def global-auto-inc (partial swap! (atom 0) inc))
(defonce !conn #?(:clj (d/create-conn {})
                  :cljs nil))

(defn transact! [conn entity]
  (d/transact! conn [entity])
  nil)

(p/defn Todo-list []
  (let [db (p/watch !conn)]
    ~@(dom/div
        (dom/h1 (dom/text "Todo list - collaborative"))
        (ui/input {:dom/placeholder    "Press enter to create a new item"
                   ::ui/keychord-event [#{"enter"} (p/fn [js-event]
                                                     (when js-event
                                                       (let [dom-node (:target js-event)
                                                             value    (:value dom-node)]
                                                         (dom/oset! dom-node :value "")
                                                         ~@(transact! !conn {:db/id            (global-auto-inc)
                                                                             :task/description value
                                                                             :task/status      :active}))))]})

        (dom/div
          (p/for [id ~@(d/q '[:find [?e ...] :in $ :where [?e :task/status]] db)]
            (dom/label
              (dom/set-style! dom/node :display "block")
              ~@(let [e      (d/entity db id)
                      status (:task/status e)]
                  ~@(do (ui/checkbox {:dom/checked     (case status :active false, :done true)
                                      ::ui/input-event (p/fn [js-event]
                                                         (when js-event
                                                           (let [done? (dom/oget js-event :target :checked)]
                                                             ~@(transact! !conn {:db/id       id
                                                                                 :task/status (if done? :done :active)}))))})
                        (dom/span (dom/text (str ~@(:task/description e)))))))))
        (dom/p (dom/text (str ~@(count (d/q '[:find [?e ...] :in $ ?status :where [?e :task/status ?status]] db :active))
                              " items left"))))))

(def main #?(:cljs (p/boot (try (binding [dom/node (dom/by-id "root")]
                                  ~@(Todo-list.))
                                (catch Pending _)))))
