(ns hfdl.impl.rfor
  (:require [hfdl.impl.util :as u])
  (:import (clojure.lang IFn IDeref)
           (java.util.concurrent.atomic AtomicReference AtomicInteger)
           (java.util.function IntBinaryOperator)))

;; outer: prev, iter, k->b, s->v, queue, ready, notifier, terminator, continuation
;; branch: prev, iter, slot
;; patch: prev, barrier, adds, movs, rets
;; undefined behavior: empty child flows, failing flows, when items in parent collection are not distinct
(def rfor
  (let [prev (int 0)
        iterator (int 1)
        store (int 2)
        result (int 3)
        queue (int 4)
        ready (int 5)
        notifier (int 6)
        terminator (int 7)
        continuation (int 8)
        slot (int 2)
        barrier (int 1)
        adds (int 2)
        movs (int 3)
        rets (int 4)
        toggle (reify IntBinaryOperator (applyAsInt [_ x y] (bit-xor x y)))]
    (letfn [(push! [o x]
              (let [q ^AtomicReference (aget o queue)]
                (loop []
                  (let [e (.get q)]
                    (if (.compareAndSet q (aset x prev e) x)
                      (when (number? e) ((aget o notifier))) (recur))))))
            (done! [o x]
              (let [q ^AtomicReference (aget o queue)]
                (aset x iterator nil)
                (loop []
                  (let [e (.get q)]
                    (if (number? e)
                      (if (.compareAndSet q e (dec e))
                        (when (= e 1) ((aget o terminator))) (recur))
                      (if (.compareAndSet q (aset x prev e) x)
                        (do) (recur)))))))
            (more! [o]
              (when (zero? (.accumulateAndGet ^AtomicInteger (aget o ready) (int 1) toggle))
                (if-some [c (aget o continuation)]
                  (let [ks @(aget o iterator)               ;; TODO errors
                        p (object-array 5)]
                    (doto p
                      (aset barrier (AtomicInteger.))
                      (aset adds {})
                      (aset movs {})
                      (aset rets (aget o store)))
                    (reduce (fn [s k]
                              (let [a (aget p adds)
                                    m (aget p movs)
                                    r (aget p rets)]
                                (if-some [b (get r k)]
                                  (doto p
                                    (aset movs (if (= s (aget b slot)) m (assoc m s b)))
                                    (aset rets (dissoc r k)))
                                  (let [b (object-array 3)]
                                    (doto b
                                      (aset prev b)
                                      (aset slot s)
                                      (aset iterator
                                        ((c k)
                                         #(if (identical? b (aget b prev))
                                            (when (zero? (.decrementAndGet ^AtomicInteger (aget p barrier)))
                                              (push! o p) (more! o)) (push! o b))
                                         #(if (identical? b (aget b prev))
                                            (comment TODO child flow is empty, do something)
                                            (done! o b)))))
                                    (doto p (aset adds (assoc a k b)))))
                                (inc s))) 0 ks)
                    (aset o store
                      (reduce dissoc
                        (reduce-kv assoc
                          (aget o store)
                          (aget p adds))
                        (keys (aget p rets))))
                    (when (zero? (.addAndGet ^AtomicInteger (aget p barrier) (count (aget p adds))))
                      (push! o p) (recur o)))
                  (done! o o))))
            (pull! [o l e]
              (let [p (u/aget-aset e prev nil)]
                (if-some [i (aget e iterator)]
                  (if (= 5 (alength e))
                    (let [l (+ l (count (aget e adds)))
                          p (reduce-kv (fn [p _ b]
                                         (aset o result (conj (aget o result) nil))
                                         (doto b (aset prev p))) p (aget e adds))]
                      (reduce-kv (fn [_ s b]
                                   (let [r (aget o result)]
                                     (aset o result (assoc r s (nth r (aget b slot))))
                                     (aset b slot s))) nil (aget e movs))
                      (reduce-kv (fn [_ _ b]
                                   ((aget b iterator))
                                   (aset o result (pop (aget o result)))
                                   (aset b slot nil)) nil (aget e rets))
                      (if (nil? p) l (recur o l p)))
                    (do
                      (if-some [s (aget e slot)]
                        (aset o result (assoc (aget o result) s @i)) ;; TODO errors
                        (try @i (catch Throwable _)))
                      (if (nil? p) l (recur o l p))))
                  (let [l (dec l)]
                    (if (nil? p) l (recur o l p))))))]
      (fn [cont flow]
        (fn [n t]
          (let [o (object-array 9)]
            (doto o
              (aset continuation cont)
              (aset ready (AtomicInteger.))
              (aset queue (AtomicReference. 1))
              (aset notifier n)
              (aset terminator t)
              (aset iterator (flow #(more! o) #(doto o (aset continuation nil) (more!))))
              (aset store {})
              (aset result []))
            (more! o)
            (reify
              IFn
              (invoke [_]
                ;; TODO
                )
              IDeref
              (deref [_]
                (let [q ^AtomicReference (aget o queue)]
                  (loop [tail nil
                         live 0]
                    (let [head (.get q)
                          live (loop [l live n nil e head]
                                 (let [p (u/aget-aset e prev n)]
                                   (if (number? p)
                                     (pull! o (+ l p) e)
                                     (if (identical? tail p)
                                       (pull! o l e) (recur l e p)))))]
                      (if (.compareAndSet q head live)
                        (when (zero? live) ((aget o terminator))) (recur head live)))))
                (aget o result)))))))))

(comment
  (require '[missionary.core :as m])
  (def >control (m/watch (doto (atom [1 2 3]) (->> (def !)))))
  (def >control (m/observe #(do (def ! %) u/nop)))
  (def !targets (repeatedly #(atom 0)))
  (def it
    ((rfor (fn [x] (m/watch (nth !targets x))) >control)
     #(prn :ready) #(prn :done)))

  (! [2])
  @it
  (! [])

  (swap! (nth !targets 2) inc)
  (swap! (nth !targets 3) inc)

  (def it ((rfor (fn [_]) (m/ap ())) #(prn :ready) #(prn :done)))

  )