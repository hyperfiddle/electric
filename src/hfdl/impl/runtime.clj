(ns hfdl.impl.runtime
  (:require [missionary.core :as m])
  (:import (java.util.concurrent.atomic AtomicReferenceArray AtomicInteger AtomicReference)
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

(defn nop [])

(defn swap-token! [^AtomicReference r t]
  (loop []
    (if-some [x (.get r)]
      (if (.compareAndSet r x t)
        (do) (recur)) (t))))

;; TODO error handling
(defn join "
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
    (let [state (AtomicReference. nop)
          token (AtomicReference. nop)
          alive (AtomicInteger. 1)
          done! #(when (zero? (.decrementAndGet alive)) (t))
          outer (input
                  #(if-some [s (.get state)]
                     (if (.compareAndSet state s nil)
                       (if (instance? Box s) ((.-val ^Box s)) (do (s) (n))) (recur))
                     (if (.compareAndSet state nil state) (do) (recur)))
                  #(do (loop []
                         (if-some [s (.get state)]
                           (swap-token! token (if (instance? Box s) (.-val ^Box s) s))
                           (if (.compareAndSet state nil token)
                             (do) (recur)))) (done!)))]
      (.set token outer)
      (reify
        IFn
        (invoke [_]
          (loop []
            (when-some [t (.get token)]
              (if (.compareAndSet token t nil)
                (t) (recur)))))
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
                         (do (swap-token! token i) (.set state i) i))))))))))))

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
