(ns hfdl.rt
  (:require [missionary.core :as m])
  (:import (clojure.lang IRef IDeref IFn)
           (java.util.concurrent.atomic AtomicReferenceArray)))

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

(defn debug!
  "Runs given dataflow program in debug mode and returns a process instance."
  [program]
  (let [vols (doto (AtomicReferenceArray. 2)
                   (.set 0 {:status :running})
                   (.set 1 {}))
        vars (doto (object-array 1))
        public (reify
                 IFn
                 (invoke [_]
                   ((aget vars 0)))
                 IDeref
                 (deref [_]
                   (.get vols 0))
                 IRef
                 (getWatches [_]
                   (.get vols 1))
                 (addWatch [this k cb]
                   (loop []
                     (let [w (.get vols 1)]
                       (if (.compareAndSet vols 1 w (assoc w k cb))
                         this (recur)))))
                 (removeWatch [this k]
                   (loop []
                     (let [w (.get vols 1)]
                       (if (.compareAndSet vols 1 w (dissoc w k))
                         this (recur))))))
        event! (fn [f & args]
                 (let [x (.get vols 0)
                       y (apply f x args)]
                   (.set vols 0 y)
                   (reduce-kv (fn [_ k cb] (cb public k x y))
                     nil (.get vols 1))))]
    (aset vars 0
      ((m/reactor (program))
       (fn [_] (event! assoc :status :terminated))
       (fn [e] (event! assoc :status :crashed :error e))))
    public))