(ns hfdl.lib
  (:require [hfdl.lang :refer [dataflow]]
            [missionary.core :as m]
            [clojure.set :as set])
  (:import (clojure.lang IFn Box IDeref)
           (java.util.concurrent.atomic AtomicReference AtomicInteger)))

(defmacro $ [f & args]
  `(deref (~f ~@(map (partial list `unquote) args))))

(defmacro ifn [args & body]
  `(fn ~args (dataflow (let [~@(mapcat (juxt identity (partial list `deref)) args)] ~@body))))

(defn place! "
Defines a new identity representing a variable initialized with given value and usable both :
* as a continuous flow producing successive states of the variable.
* as a one-argument function assigning a new state to the variable."
  [init]
  (let [!a (atom init)
        >a (m/watch !a)]
    (fn
      ([x] (reset! !a x))
      ([n t] (>a n t)))))

(defn diff [z -]
  (fn [rf]
    (let [p (Box. z)]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r x]
         (let [r (rf r (- x (.-val p)))]
           (set! (.-val p) x) r))))))

;; TODO error handling + cancellation
(def ^{:arglists '([f in])
       :doc "
Turns a continuous flow of collections of distinct items into a flow calling given function for each item added to the
collection. The function must return another flow that will be run in parallel and cancelled when the item is removed
from the input collection. Returns a continuous flow of collections of current values of inner flows, in the same order
as input.
"} reactive-for
  (let [position (int 0)
        iterator (int 1)
        previous (int 2)
        length (int 3)]
    (letfn [(spawn! [queue notifier terminator flow pos]
              (let [this (object-array length)
                    it (flow #(if (nil? (aget this iterator))
                                (aset this iterator this)
                                (let [q (.get queue)]
                                  (aset this previous
                                    (if (identical? q queue) nil q))
                                  (if (.compareAndSet queue q this)
                                    (when (nil? q) (notifier)) (recur))))
                         terminator)]
                (when (nil? (aget this iterator))
                  (throw (ex-info "Fan-in failure : flow has no initial value." {})))
                (doto this
                  (aset position pos)
                  (aset iterator it))))]
      (fn [f in]
        (fn [n t]
          (let [store (Box. {})
                state (Box. nil)
                queue (AtomicReference. nil)
                alive (AtomicInteger. 1)
                done! #(when (zero? (.decrementAndGet alive)) (t))
                kill! (fn [_ id item]
                        ((aget item iterator))
                        (set! (.-val store)
                          (dissoc (.-val store) id)))]
            (.set queue (spawn! queue n done! in -1)) (n)
            (reify
              IFn
              (invoke [_]
                (comment TODO))
              IDeref
              (deref [_]
                (loop []
                  (loop [head (.getAndSet queue queue)]
                    (let [pos  (aget head position)
                          prev (aget head previous)
                          iter (aget head iterator)]
                      (aset head previous nil)
                      (if (neg? pos)
                        (let [s (.-val state)]
                          (set! (.-val state) [])
                          (->> @iter
                            (reduce
                              (fn [rets id]
                                (let [pos (count (.-val state))]
                                  (if-some [item (get (.-val store) id)]
                                    (do (set! (.-val state)
                                          (conj (.-val state)
                                            (nth s (aget item position))))
                                        (aset item position pos)
                                        (dissoc rets id))
                                    (let [item (spawn! queue n done! (f id) pos)]
                                      (.incrementAndGet alive)
                                      (set! (.-val state) (conj (.-val state) @(aget item iterator)))
                                      (set! (.-val store) (assoc (.-val store) id item)) rets))))
                              (.-val store))
                            (reduce-kv kill! nil)))
                        (set! (.-val state) (assoc (.-val state) pos @iter)))
                      (when-not (nil? prev) (recur prev))))
                  (if (.compareAndSet queue queue nil)
                    (.-val state) (recur)))))))))))

(comment
  (def >input (atom (vec (range 5))))
  (def ats (vec (repeatedly 10 #(atom 0))))

  ;; (A -> Flow[B]) -> Flow[Traversable[A]] -> Flow[Traversable[B]]
  (def it ((reactive-for (fn [id] (prn :branch id) (m/watch (nth ats id))) (m/watch >input))
           #(prn :ready) #(prn :done)))

  (swap! >input conj 5)
  (swap! >input pop)

  (swap! (nth ats 1) inc)
  (swap! (nth ats 2) + 2)

  (reset! >input [0 2 1 3 4 5])

  @it

  )

(comment
  (def !db (atom [1]))
  (def >db (m/watch !db))

  (defn q [query >db]
    >db)

  (defn entity-ks [>db x]
    (m/ap [:a :b :c]))

  (defn entity-get [>db x k]
    (m/ap (case k
            :a "a"
            :b "b"
            :c "c")))

  (defn get-column [x k]
    (dataflow (pr-str @(entity-get >db x k))))

  (defn get-row [x]
    (prn :get-row x)
    (dataflow
      (pr-str x)
      (let [ks @(entity-ks >db x)]
        [:pre (pr-str @(entity-get >db x :person/name))]
        @(reactive-for (partial get-column x) ~ks))))

  (def query '[:select])

  (def app
    (dataflow
      (let [xs @(q query >db)]
        [:div
         [:pre (pr-str xs)]
         @(reactive-for get-row ~xs)])))

  (require '[hfdl.lang :refer [debug!]])
  (def p (debug! app))

  (swap! !db conj 4)
  (swap! !db disj 3)
  @p

  )

(comment
  (require '[geoffrey.diff :refer [*$*]])
  (require '[datascript.core :as d])
  (geoffrey.diff/init-datascript)

  (def !db (atom *$*))
  (def >db (m/watch !db))

  ;(defn q [>db]
  ;  (m/latest #(datomic.api/q '[:find ?e :where [?e :dustingetz/email]] %) >db))

  (def query-users '[:find [?e ...] :where [?e :dustingetz/email]])
  (defn q [>db]
    (dataflow (sort (d/q query-users @>db))))

  (comment
    (sort (d/q query-users @!db)) := [9 10 11 12]
    )

  (defn entity-ks [>db x]
    (dataflow (keys (d/touch (d/entity @>db x)))))

  (keys (d/touch (d/entity @!db 10)))

  (defn entity-get [>db x kf]
    (dataflow (kf (d/entity @>db x))))

  (defn get-column [x k]
    (dataflow [:pre (pr-str @(entity-get >db x k))]))

  (defn get-row [x]
    (prn :get-row x)
    (dataflow
      #_(pr-str x)
      [:pre (pr-str @(entity-get >db x :dustingetz/email))]
      #_(let [#_#_db2 (:db-after (d/with @>db [{:dustingetz/email "dan@example.com"}]))
            ks @(entity-ks @>db x)]
        @(reactive-for (partial get-column x) ~ks))))

  (def app
    (dataflow
      (let [xs @(q >db)]
        [:div
         [:pre (pr-str xs)]
         @(reactive-for get-row ~xs)])))

  (require '[hfdl.lang :refer [debug!]])
  (def p (debug! app))

  (swap! !db #(:db-after (d/with % [{:dustingetz/email "dan@example.com"}])))

  (swap! !db #(:db-after
                (d/with %
                  [[:db/add 12 :dustingetz/email "dan2@example.com"]
                   [:db/retractEntity 11]])))

  @p

  )