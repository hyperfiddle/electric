(ns hfdl.impl.debug
  (:require [missionary.core :as m]
            [hfdl.impl.util :as u]
            [hfdl.impl.compiler :as c]
            [hfdl.sourcemap :as sm])
  (:import (java.util.concurrent.atomic AtomicReference)
           (clojure.lang IFn IDeref IRef Box)))

(declare frame!)

(deftype Context [on-frame path ^:unsynchronized-mutable ^int counter]
  IFn
  (invoke [_ df n t]
    (let [ar (frame! on-frame
               (conj path
                 (doto counter
                   (->> unchecked-inc-int
                     (set! counter))))
               (:graph df))
          it ((aget ar (:result df)) n t)]
      (reify
        IFn
        (invoke [_]
          (dotimes [i (alength ar)]
            ((aget ar i))))
        IDeref
        (deref [_] @it)))))

(defn frame! [on-frame path graph]
  (let [ar (object-array (count graph))]
    (reduce-kv
      (fn [_ idx [op & args]]
        (->> (case op
               :apply
               (apply m/latest u/call (aget ar (first args)) (map (partial aget ar) (second args)))
               :local
               (u/pure (first args))
               :global
               (u/pure @(resolve (first args)))
               :constant
               (u/pure (aget ar (first args)))
               :variable
               (c/bind-context
                 (->Context on-frame (conj path idx) 0)
                 (u/join (aget ar (first args)))))
          (m/signal!)
          (aset ar idx)))
      nil graph)
    (on-frame path graph ar)
    ar))

(defn debug! [dataflow {:keys [:source-mapped] :or {source-mapped false}}]
  (let [state (AtomicReference. ^{:program dataflow} {:status :running, :log []})
        watches (AtomicReference. {})
        reactor (Box. nil)
        public (reify
                 IFn
                 (invoke [_]
                   ((.-val reactor)))
                 IDeref
                 (deref [_]
                   (.get state))
                 IRef
                 (getWatches [_]
                   (.get watches))
                 (addWatch [this k cb]
                   (loop []
                     (let [w (.get watches)]
                       (if (.compareAndSet watches w (assoc w k cb))
                         this (recur)))))
                 (removeWatch [this k]
                   (loop []
                     (let [w (.get watches)]
                       (if (.compareAndSet watches w (dissoc w k))
                         this (recur))))))
        event! (fn [f & args]
                 (let [x (.get state)
                       y (apply f x args)]
                   (.set state y)
                   (reduce-kv (fn [_ k cb] (cb k public x y))
                     nil (.get watches))))
        humanizef (sm/humanize dataflow)]
    (set! (.-val reactor)
      ((m/reactor
         (->> (fn [!] (frame! (fn [path graph frame]
                                (! (into {path (u/pure graph)}
                                     (map (juxt
                                            (partial conj path)
                                            (partial aget frame)))
                                     (range (alength frame)))))
                        [] (:graph dataflow)) u/nop)
           (m/observe)
           (m/relieve merge)
           (u/slot-changes)
           (m/relieve merge)
           (m/stream!)
           (u/foreach #(m/sp (event! update :log conj (if source-mapped
                                                        (with-meta (humanizef %) %)
                                                        %))))
           (m/stream!)))
       (fn [_] (event! assoc :status :terminated))
       (fn [e] (event! assoc :status :crashed :error e))))
    public))