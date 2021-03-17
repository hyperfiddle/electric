(ns hfdl.impl.runtime
  (:require [missionary.core :as m])
  (:import (java.util.concurrent.atomic AtomicReferenceArray)
           (clojure.lang IFn IDeref IRef)))

(defn call
  ([f] (f))
  ([f a] (f a))
  ([f a b] (f a b))
  ([f a b c] (f a b c))
  ([f a b c & ds] (apply f a b c ds)))

(defn pure [x]
  (fn [n t]
    (n) (reify
          IFn
          (invoke [_])
          IDeref
          (deref [_] (t) x))))

(defn join [x]
  (comment TODO))

(defn spawn [x]
  (comment TODO))

(defn build! [frame]
  (reduce-kv
    (fn [arr idx [op & args]]
      (let [flow (case op
                   :spawn
                   (spawn (aget arr (first args)))
                   :apply
                   (apply m/latest call (aget arr (first args)) (map (partial aget arr) (second args)))
                   :local
                   (pure (first args))
                   :global
                   (pure (resolve (first args)))
                   :constant
                   (pure (aget arr (first args)))
                   :variable
                   (join (aget arr (first args))))]
        (doto arr (aset idx (m/signal! flow)))))
    (object-array (count frame)) frame))

(defn debug! [{:keys [frame]}]
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
      ((m/reactor
         (comment TODO)
         (build! frame))
       (fn [_] (event! assoc :status :terminated))
       (fn [e] (event! assoc :status :crashed :error e))))
    public))