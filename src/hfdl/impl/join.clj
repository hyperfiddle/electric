(ns hfdl.impl.join
  (:require [hfdl.impl.util :as u]
            [hfdl.impl.token :as t])
  (:import (java.util.concurrent.atomic AtomicReference AtomicInteger)
           (clojure.lang Box IFn IDeref)))

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
          token (t/token)
          alive (AtomicInteger. 1)
          done! #(when (zero? (.decrementAndGet alive)) (t))
          outer (input
                  #(if-some [s (.get state)]
                     (if (.compareAndSet state s nil)
                       (if (instance? Box s) ((.-val ^Box s)) (do (s) (n))) (recur))
                     (if (.compareAndSet state nil state) (do) (recur)))
                  #(do (loop []
                         (if-some [s (.get state)]
                           (t/swap-token! token (if (instance? Box s) (.-val ^Box s) s))
                           (if (.compareAndSet state nil token)
                             (do) (recur)))) (done!)))]
      (.set token outer)
      (reify
        IFn
        (invoke [_]
          (t/burn-token! token))
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
                         (do (t/swap-token! token i) (.set state i) i))))))))))))
