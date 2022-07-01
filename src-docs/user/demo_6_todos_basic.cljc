(ns user.demo-6-todos-basic
  (:require clojure.edn
            [datascript.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  (:import [hyperfiddle.photon Pending]
           missionary.Cancelled)
  #?(:cljs (:require-macros user.demo-6-todos-basic)))

;;; Business logic

(def auto-inc (partial swap! (atom 0) inc)) ; when called, swaps the atom and return the swapped value, so 1, then 2, then 3, …

(defn task-create [description]
  [{:db/id            (auto-inc)
    :task/description description
    :task/status      :active}])

(defn task-status [id status]
  [{:db/id       id
    :task/status status}])

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

(p/def db)                              ; will be bound on server

(p/defn Todo-list [db]
  (let [time-basis (:max-tx db)]        ; latest tx time, used to acknowledge a value has been saved (transacted) on server
    ~@(dom/div
        (dom/h1 (dom/text "Todo list - basic"))
        (ui/input {:placeholder "Press enter to create a new item"
                   :on-keychord
                   [time-basis     ; acknowledgement ; TODO remove from userland
                    #{"enter"}     ; key combo(s) to listen to
                    (p/fn [js-event]
                      (when js-event
                        (let [dom-node    (dom/oget js-event :target)
                              description (get-input-value dom-node)]
                          (clear-input! dom-node)
                          ;; Create a task-create transaction. It is returned by
                          ;; ui/input and will bubble up to the top.
                          (task-create description)
                          )))]})
        (dom/div
          (p/for [id ~@(d/q '[:find [?e ...] :in $ :where [?e :task/status]] db)]
            (dom/label {:style {:display :block}}
              (ui/checkbox
                {:checked  (case ~@(:task/status (d/entity db id))
                             :active false
                             :done   true)
                 :on-input [time-basis   ; acknowledgement
                            (p/fn [js-event]
                              (when js-event
                                (let [checked? (dom/oget js-event :target :checked)]
                                  ;; Return a task-status tx. It is returned by
                                  ;; ui/checkbox and will bubble up to the top.
                                  (task-status id (if checked? :done :active)))))]})
              (dom/span (dom/text (str ~@(:task/description (d/entity db id))))))))
        (dom/p
          (dom/text (str ~@(count (d/q '[:find [?e ...] :in $ ?status
                                         :where [?e :task/status ?status]]
                                    db :active)) " items left"))))))

(defn transact [tx-data] #?(:clj (do (prn `transact tx-data)
                                     (d/transact! !conn tx-data)
                                     nil)))


(p/defn App []
  ~@(if-some [tx (p/deduping (seq (Todo-list. (p/watch !conn))))]
      (do (prn "TX:" tx) (transact tx)) ; auto-transact, prints server-side
      (prn :idle)))

(def main #?(:cljs (p/client (p/main (try (binding [dom/node (dom/by-id "root")]
                                            (App.))
                                          (catch Pending _)
                                          (catch Cancelled _))))))

(comment
  (user/browser-main! `main)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (comment
;;   (def !ack (atom 0))

;;   ;; Works
;;   (def main #?(:cljs (p/client (p/main (try (binding [dom/node (dom/by-id "root")]
;;                                               ~@(when-some [v ~@(dom/button {} (z/impulse ~@(p/watch !ack)
;;                                                                                  (dom/>events "click" (map (constantly :click)))))]
;;                                                   (do (prn "got" v)
;;                                                       (swap! !ack inc))))
;;                                             (catch Pending _)
;;                                             (catch Cancelled _))))))

;;   (p/defn UI [ack]
;;     (dom/button {} (z/impulse ack (dom/>events "click" (map (constantly :click))))))

;;   ;; Infinite loop
;;   #_(def main #?(:cljs (p/client (p/main (try (binding [dom/node (dom/by-id "root")]
;;                                                 ~@(when-some [v ~@(UI. ~@(p/watch !ack))]
;;                                                     (do (prn "got" v)
;;                                                         (swap! !ack inc))))
;;                                               (catch Pending _)
;;                                               (catch Cancelled _)))))))

