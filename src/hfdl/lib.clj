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
Turns a continuous flow of sets into a flow calling given function for each item added to the set. The function must
return another flow that will be run in parallel and cancelled when the item is removed from the input set. Returns a
continuous flow of sets of current values of inner flows.
"} reactive-for
  (let [identity (int 0)
        iterator (int 1)
        previous (int 2)
        length (int 3)]
    (letfn [(spawn! [queue notifier terminator flow]
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
                (doto this (aset iterator it))))]
      (fn [f in]
        (fn [n t]
          (let [queue (AtomicReference. nil)
                alive (AtomicInteger. 1)
                done! #(when (zero? (.decrementAndGet alive)) (t))
                store (Box. {})
                state (Box. {})
                check (fn [adds id iter]
                        (let [adds-id (disj adds id)]
                          (when (= adds-id adds)
                            (set! (.-val state) (dissoc (.-val state) id))
                            (set! (.-val store) (dissoc (.-val store) id))
                            (iter)) adds-id))
                inner (fn [prev id]
                        (.incrementAndGet alive)
                        (doto (spawn! queue n done! (f id))
                          (aset identity id)
                          (aset previous prev)
                          (->
                            (aget iterator)
                            (->>
                              (assoc (.-val store) id)
                              (set! (.-val store))))))]
            (.set queue
              (doto (spawn! queue n done! in)
                (aset identity spawn!))) (n)
            (reify
              IFn
              (invoke [_]
                (comment TODO))
              IDeref
              (deref [_]
                (loop []
                  (loop [head (.getAndSet queue queue)]
                    (let [id   (aget head identity)
                          prev (aget head previous)
                          iter (aget head iterator)]
                      (aset head previous nil)
                      (let [head (if (identical? id spawn!)
                                   (->> (.-val store)
                                     (reduce-kv check @iter)
                                     (reduce inner prev))
                                   (do (set! (.-val state)
                                         (assoc (.-val state)
                                           id @iter)) prev))]
                        (when-not (nil? head) (recur head)))))
                  (if (.compareAndSet queue queue nil)
                    (set (vals (.-val state))) (recur)))))))))))

(comment
  (def >input (atom #{1 2 3}))
  (def ats (vec (repeatedly 10 #(atom 0))))

  ;; (A -> Flow[B]) -> Flow[Set[A]] -> Flow[Set[B]]
  (def it ((reactive-for (fn [id] (m/watch (nth ats id))) (m/watch >input))
           #(prn :ready) #(prn :done)))

  (swap! >input conj 4)
  (swap! >input disj 2)

  (swap! (nth ats 1) inc)
  (swap! (nth ats 2) + 2)

  @it

  )

(comment
  (def !db (atom #{1}))
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
      #_
      (let [ks @(entity-ks >db x)]
        [:pre (pr-str @(entity-get >db x :person/name))]
        @(reactive-for (partial get-column >x) ~ks))))

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