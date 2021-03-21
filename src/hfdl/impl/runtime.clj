(ns hfdl.impl.runtime
  (:require [missionary.core :as m]
            [hfdl.impl.compiler :refer [with-ctx]])
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

(defn nop [])

(defn swap-token! [^AtomicReference r t]
  (loop []
    (if-some [x (.get r)]
      (if (.compareAndSet r x t)
        (do) (recur)) (t))))

(defn cancel-frame! [ar]
  (dotimes [i (alength ar)]
    ((aget ar i))))

"
legacy join doc, TODO error handling

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
"
(defn boot [on-slot]
  (fn frame! [path graph]
    (let [ar (object-array (count graph))]
      (reduce-kv
        (fn [_ idx [op & args]]
          (let [path (conj path idx)]
            (->> (case op
                   :apply
                   (apply m/latest call (aget ar (first args)) (map (partial aget ar) (second args)))
                   :local
                   (pure (first args))
                   :global
                   (pure (resolve (first args)))
                   :constant
                   (pure (aget ar (first args)))
                   :variable
                   (fn [n t]
                     (let [state (AtomicReference. nop)
                           token (AtomicReference. nop)
                           alive (AtomicInteger. 1)
                           done! #(when (zero? (.decrementAndGet alive)) (t))
                           outer ((aget ar (first args))
                                   #(if-some [s (.get state)]
                                      (if (.compareAndSet state s nil)
                                        (if (instance? Box s) ((.-val ^Box s)) (do (s) (n))) (recur))
                                      (if (.compareAndSet state nil state) (do) (recur)))
                                   #(do (loop []
                                          (if-some [s (.get state)]
                                            (swap-token! token (if (instance? Box s) (.-val ^Box s) s))
                                            (if (.compareAndSet state nil token)
                                              (do) (recur)))) (done!)))
                           counter (int-array 1)
                           context (fn [dataflow n t]
                                     (let [ar (frame!
                                                (conj path
                                                  (doto (aget counter 0)
                                                    (->> unchecked-inc-int
                                                      (aset counter 0))))
                                                (:graph dataflow))
                                           it ((aget ar (:result dataflow)) n t)]
                                       (reify
                                         IFn
                                         (invoke [_] (cancel-frame! ar))
                                         IDeref
                                         (deref [_] @it))))]
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
                           (with-ctx context
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
                                            (do (swap-token! token i) (.set state i) i)))))))))))))
              (m/signal!)
              (aset ar idx)
              (on-slot path))))
        nil graph) ar)))

(defn foreach [f input]
  (m/ap (m/? (f (m/?? input)))))

(defn slot-changes [slots]
  (m/ap
    (let [[path slot] (m/?= (m/enumerate (m/?= slots)))]
      {path (m/?? slot)})))

(defn debug! [dataflow]
  (let [state (AtomicReference. {:status :running :log []})
        watches (AtomicReference. {})
        reactor (Box. nil)
        on-slot (Box. nil)
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
                     nil (.get watches))))]
    (set! (.-val reactor)
      ((m/reactor
         (let [slot-feed (->> (m/observe (fn [!] (set! (.-val on-slot) (comp ! hash-map)) nop))
                           (m/relieve merge)
                           (m/stream!))]
           ((boot (.-val on-slot)) [] (:graph dataflow))
           (->> slot-feed
             (slot-changes)
             (m/relieve merge)
             (m/stream!)
             (foreach #(m/sp (event! update :log conj %)))
             (m/stream!))))
       (fn [_] (event! assoc :status :terminated))
       (fn [e] (event! assoc :status :crashed :error e))))
    public))