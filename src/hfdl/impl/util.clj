(ns hfdl.impl.util
  (:require [missionary.core :as m])
  (:import (java.util.concurrent.atomic AtomicReference AtomicInteger)
           (clojure.lang IFn IDeref Box)))

(defn nop [])

(defn token
  ([] (token nop))
  ([c] (AtomicReference. c)))

(defn swap-token! [^AtomicReference t c]
  (loop []
    (if-some [x (.get t)]
      (if (.compareAndSet t x c)
        (do) (recur)) (c))))

(defn burn-token! [^AtomicReference t]
  (loop []
    (when-some [c (.get t)]
      (if (.compareAndSet t c nil)
        (c) (recur)))))

(defn pure [x]
  (fn [n t]
    (n) (reify
          IFn
          (invoke [_])
          IDeref
          (deref [_] (t) x))))

(def map-into (partial mapv into))
(def swap (juxt second first))

(defn monoid [f i]
  (fn
    ([] i)
    ([x] x)
    ([x y] (f x y))
    ([x y & zs] (reduce f (f x y) zs))))

(defn call
  ([f] (f))
  ([f a] (f a))
  ([f a b] (f a b))
  ([f a b c] (f a b c))
  ([f a b c & ds] (apply f a b c ds)))

(defmacro aget-aset [arr idx val]
  `(let [a# ~arr
         i# ~idx
         x# (aget a# i#)]
     (aset a# i# ~val) x#))

(defn pst [^Throwable e]
  (.printStackTrace e))

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
    (let [state (AtomicReference. nop)
          token (token)
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
          (burn-token! token))
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

(defn slot-changes [slots]
  (m/ap
    (let [[path slot] (m/?= (m/enumerate (m/?= slots)))]
      {path (try (m/?? slot) (catch Throwable _ ::cancelled))})))

(defn foreach [f input]
  (m/ap (m/? (f (m/?? input)))))

(defn poll [task]
  ; takes a task and returns a flow that runs the task repeatedly
  (m/ap (m/? (m/?? (m/enumerate (repeat task))))))

(defmacro amb= [& forms]
  `(case (m/?= (m/enumerate (range ~(count forms))))
     ~@(interleave (range) forms)))