(ns user.controlled-inputs
  (:require #?(:clj [datomic.api :as d])
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [swinged.rosie :as rosie])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros user.controlled-inputs)))

; Depends on Rosie bindings - todo decouple

#?(:clj (defn slow-identity [x] (Thread/sleep 500) x))

(def edward 694891374557546)

(p/defn DemoInput []
  (p/client
    (dom/h1 "a controlled input that reverts on blur")
    (let [a (dom/Input. "hello world")]
      (dom/div a))


    (dom/h1 "a controlled input with looped state")
    (let [a (p/with-cycle [a "hello world"]
              (dom/Input. a))]
      (dom/div a))

    ; loop/recur
    (dom/h1 "a controlled input with looped state")
    (let [a (loop [a "hello world"]
              (recur (dom/Input. a)))]
      (dom/div a))

    (dom/h1 "controlled input - edn")
    (let [a (loop [a :category/terminated-contract]
              (recur (-> (dom/Input. (pr-str a)) rosie/read-edn-str)))]
      (dom/div a))

    ; data State a = [Boolean, Option a]

    (dom/h1 "controlled input with pending; disable input while waiting")
    (let [a (:ret (p/with-cycle [{:keys [busy] :as s} {:busy false
                                                       :ret nil}]
                    (println 's s)
                    (try {:busy false
                          :ret (p/server (identity (p/client (p/with-cycle [a "hello world"]
                                                               (dom/pre (pr-str {:stale busy}))
                                                               (dom/Input. a #_{:disabled busy})))))}
                         (catch Pending _
                           (println 'pending)
                           (assoc s :busy true)))))] ; :ret is stale - latency
      (dom/div a))

    ; L: it will do something weird
    ; 1. reset atom
    ; 2. run in parallel and throw pending
    ; 3 print pending, reset busy true
    ; result is true, returned by reset
    ; frame continues to propoagate
    ; at the end of the propagation, another frame is run in response to !busy changing to true, which was changed by downwards which is a cycle.
    ;     reactor is about dags, so if you build a cycle artificially, it schedules a new propagation turn
    ;
    (let [a (let [!busy (atom false) busy (p/watch !busy)]
              (println 'busy)
              (reset! !busy
                      (try (p/server (identity (p/client (p/with-cycle [a "hello world"]
                                                           (dom/pre (pr-str {:stale busy}))
                                                           (dom/Input. a #_{:disabled busy})))))
                           false
                        (catch Pending _
                          (println 'pending)
                          true))))]
      (dom/div a))

    (let [a (loop [busy false]
              (println 'busy)
              (try (recur false) ; recur in non-tail position due to implicit do
                   (p/server (identity (p/client (loop [a "hello world"]
                                                   (dom/pre (pr-str {:stale busy}))
                                                   (recur (dom/Input. a #_{:disabled busy}))))))
                   (catch Pending _
                     (println 'pending)
                     (recur true))))] ; :ret is stale - latency
      (dom/div a))
    ; L: what does it mean to recur twice, these are simultaneous under FRP

    ; can it be factored into tail position?
    ; does it matter if it's in the tail position like it does for Clojure?





    ;(let [!x (atom nil) x (p/watch !x)]
    ;  (reset! (do ...)))
    ;
    ;
    ;(dom/h1 "controlled input with pending; disable input while waiting")
    ;(let [a (loop [busy false]
    ;          (try (if busy (recur (cons history false)))
    ;               (p/server (identity (p/client (p/with-cycle [a "hello world"]
    ;                                               (dom/pre (pr-str {:stale busy}))
    ;                                               (dom/Input. a #_{:disabled busy})))))
    ;               (catch Pending _
    ;                 (recur true))))]
    ;  (dom/div a))


    (dom/h1 "controlled input - database backed (latency) - google display name test")
    (let [a (p/with-cycle [a (p/server (:google/display-name (d/entity rosie/db edward)))]
              (dom/Input. a))]
      (when (contrib.str/blank->nil a)
        (dom/pre (pr-str [[:db/add edward :google/display-name a]]))))


    ;; loop/recur version
    ;(dom/h1 "controlled input - database backed (latency) - google display name test")
    ;(let [a (loop [a (p/server (:google/display-name (d/entity rosie/db edward)))]
    ;          (recur (dom/Input. (or a ""))))]
    ;  (when (contrib.str/blank->nil a)
    ;    (dom/pre (pr-str [[:db/add edward :google/display-name a]]))))


    (dom/h1 "controlled input with stage")
    (p/server
      (p/with-cycle [stage []]
        (let [db (:db-after (d/with rosie/db (or stage [])))
              m (d/pull db [:google/display-name :db/id] edward)
              a (:google/display-name m #_(d/entity db edward))] ; d/entity broken equality
          (p/client
            (dom/div "stage is: " (pr-str stage))
            (dom/div "entity is: " (contrib.str/pprint-str m))
            (dom/div "display-name is: " a)
            (when-some [a (contrib.str/blank->nil (dom/Input. (or a "")))]
              [[:db/add edward :google/display-name a]]))))
      nil)


    ;(dom/h1 "controlled input with stage") ; loop/recur
    ;(p/server
    ;  (loop [stage []]
    ;    (let [db (:db-after (d/with rosie/db (or stage [])))
    ;          m (d/pull db [:google/display-name :db/id] edward)
    ;          a (:google/display-name m #_(d/entity db edward))] ; d/entity broken equality
    ;      (p/client
    ;        (dom/div "stage is: " (pr-str stage))
    ;        (dom/div "entity is: " (contrib.str/pprint-str m))
    ;        (dom/div "display-name is: " a)
    ;        (when-some [a (contrib.str/blank->nil (dom/Input. (or a "")))]
    ;          (recur [[:db/add edward :google/display-name a]]))))))

    ; what if there is no base case?
    ;(loop [x true]
    ;  (println x)
    ;  (recur true)
    ;  nil)

    (dom/br)
    (dom/p "staging area")))

(comment
  (p/client

    ; 1. where do the busy indicators go and how many are there?
    ; 2. does loop/recur help? Not sure it matters

    (p/defn Frame [db a]
      (when-some [a (contrib.str/blank->nil (dom/Input. (or a "")))] ; accept repeated events, keep latest
        (when (valid? a)
          [[:db/add edward :google/display-name a]])))

    (p/server
      (loop [db rosie/db]
        (let [m (d/pull db [:google/display-name :db/id] edward)
              a (:google/display-name m #_(d/entity db edward))] ; d/entity broken equality
          (p/client
            (dom/div "db is: " (pr-str db))
            (dom/div "entity is: " (contrib.str/pprint-str m))
            (dom/div "display-name is: " a)
            (when-some [tx (Frame. db a)]
              (p/server (recur (:db-after (d/with db tx)))))))))


    (p/client
      (loop [busy false]
        (try
          ; popover body
          (dom/div {:class "popover"}
            (dom/div {:aria-busy busy})
            (p/server
              (loop [tx []]
                (let [db (:db-after (d/with rosie/db tx))
                      m (d/pull db [:google/display-name :db/id] edward)
                      a (:google/display-name m #_(d/entity db edward))] ; d/entity broken equality
                  (p/client
                    (dom/div "db is: " (pr-str db))
                    (dom/div "entity is: " (contrib.str/pprint-str m))
                    (dom/div "display-name is: " a)
                    (when-some [tx (Frame. db a)]
                      (p/server (recur tx))))))))
          (recur false)
          (catch Pending _ (recur true)))))

    ))