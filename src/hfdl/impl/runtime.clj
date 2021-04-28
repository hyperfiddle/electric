(ns hfdl.impl.runtime
  (:require [missionary.core :as m]
            [hfdl.impl.util :as u]
            [hfdl.impl.compiler :refer [with-ctx]]
            [hfdl.sourcemap :as sm])
  (:import (java.util.concurrent.atomic AtomicInteger AtomicReference)
           (clojure.lang IFn IDeref IRef Box)))

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

(defn bind-context [ctx flow]
  (fn [n t]
    (let [it (flow n t)]
      (reify
        IFn
        (invoke [_] (it))
        IDeref
        (deref [_] (with-ctx ctx @it))))))

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

(defn join "
TODO error handling

Turns a continuous flow of continuous flows into a continuous flow running successive flows produced by samplings of
outer flow and producing samplings of the latest inner flow. Inner flows must be ready to transfer immediately on start.
Resulting flow becomes ready to transfer when either the outer flow or the current inner flow becomes ready to transfer.
When the outer flow becomes ready to transfer, the inner flow currently running is cancelled. When the resulting flow is
cancelled, the outer flow is cancelled along with the final inner flow when the outer flow terminates.

Implementation :
1. `state` : transfer state.
`nil` : outer ready, inner cancelled, transfer pending
`state` : outer ready, inner bootstrapping
`token` : outer done, inner bootstrapping
`iterator` : outer not ready, inner not ready
`Box[iterator]` : outer not ready, inner ready, transfer pending
2. `token` : `nil` when flow is cancelled, otherwise the function to call on cancellation. The cancellation function is
the outer flow iterator until it's terminated, the inner flow iterator afterwards.
3. `alive` : the number of non-terminated iterators (outer + inners).
" [input]
  (fn [n t]
    (let [state (AtomicReference. u/nop)
          token (u/token)
          alive (AtomicInteger. 1)
          done! #(when (zero? (.decrementAndGet alive)) (t))
          outer (input
                  #(if-some [s (.get state)]
                     (if (.compareAndSet state s nil)
                       (if (instance? Box s) ((.-val ^Box s)) (do (s) (n))) (recur))
                     (if (.compareAndSet state nil state) (do) (recur)))
                  #(do (loop []
                         (if-some [s (.get state)]
                           (u/swap-token! token (if (instance? Box s) (.-val ^Box s) s))
                           (if (.compareAndSet state nil token)
                             (do) (recur)))) (done!)))]
      (.set token outer)
      (reify
        IFn
        (invoke [_]
          (u/burn-token! token))
        IDeref
        (deref [_]
          @(loop []
             (if-some [b (.get state)]
               (let [i (.-val ^Box b)]
                 (if (.compareAndSet state b i)
                   i (recur)))
               (loop []
                 (.incrementAndGet alive)
                 (let [b (Box. nil)
                       i (@outer
                           #(if-some [i (.-val b)]
                              (if (.compareAndSet state i b)
                                (n) (try @i (catch Throwable _)))
                              (set! (.-val b) b)) done!)]
                   (when (nil? (.-val b))
                     (throw (IllegalStateException. "Join failure : inner flow has no initial value.")))
                   (set! (.-val b) i)
                   (if (.compareAndSet state nil i)
                     i (if (.compareAndSet state state nil)
                         (do (i) (try @i (catch Throwable _)) (recur))
                         (do (u/swap-token! token i) (.set state i) i))))))))))))

(defn frame! [on-frame path graph]
  (let [ar (object-array (count graph))]
    (reduce-kv
      (fn [_ idx [op & args]]
        (->> (case op
               :apply
               (apply m/latest call (aget ar (first args)) (map (partial aget ar) (second args)))
               :local
               (pure (first args))
               :global
               (pure @(resolve (first args)))
               :constant
               (pure (aget ar (first args)))
               :variable
               (bind-context
                 (->Context on-frame (conj path idx) 0)
                 (join (aget ar (first args)))))
          (m/signal!)
          (aset ar idx)))
      nil graph)
    (on-frame path graph ar)
    ar))

(defn foreach [f input]
  (m/ap (m/? (f (m/?? input)))))

(defn slot-changes [slots]
  (m/ap
    (let [[path slot] (m/?= (m/enumerate (m/?= slots)))]
      {path (try (m/?? slot) (catch Throwable _ ::cancelled))})))

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
                                (! (into {path (pure graph)}
                                     (map (juxt
                                            (partial conj path)
                                            (partial aget frame)))
                                     (range (alength frame)))))
                        [] (:graph dataflow)) u/nop)
           (m/observe)
           (m/relieve merge)
           (slot-changes)
           (m/relieve merge)
           (m/stream!)
           (foreach #(m/sp (event! update :log conj (if source-mapped
                                                      (with-meta (humanizef %) %)
                                                      %))))
           (m/stream!)))
       (fn [_] (event! assoc :status :terminated))
       (fn [e] (event! assoc :status :crashed :error e))))
    public))