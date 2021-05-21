(ns hfdl.impl.rfor
  (:require-macros [hfdl.impl.util :as u]))

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
        rets (int 4)]
    (letfn [(push! [o x]
              (let [e (aget o queue)]
                (aset x prev e)
                (aset o queue x)
                (when (number? e)
                  ((aget o notifier)))))
            (done! [o x]
              (aset x iterator nil)
              (let [e (aget o queue)]
                (if (number? e)
                  (do (aset o queue (dec e))
                      (when (= e 1) ((aget o terminator))))
                  (do (aset x prev e)
                      (aset o queue x)))))
            (more! [o]
              (when (aset o ready (not (aget o ready)))
                (if-some [c (aget o continuation)]
                  (let [ks @(aget o iterator)               ;; TODO errors
                        p (object-array 5)]
                    (doto p
                      (aset barrier 0)
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
                                            (when (zero? (aset p barrier (dec (aget p barrier))))
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
                    (when (zero? (aset p barrier (+ (aget p barrier) (count (aget p adds)))))
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
                                   (aset o result (assoc (aget o result) s (aget b slot)))
                                   (aset b slot s)) nil (aget e movs))
                      (reduce-kv (fn [_ _ b]
                                   ((aget b iterator))
                                   (aset o result (pop (aget o result)))
                                   (aset b slot nil)) nil (aget e rets))
                      (if (nil? p) l (recur o l p)))
                    (do
                      (if-some [s (aget e slot)]
                        (aset o result (assoc (aget o result) s @i)) ;; TODO errors
                        (try @i (catch :default _)))
                      (if (nil? p) l (recur o l p))))
                  (let [l (dec l)]
                    (if (nil? p) l (recur o l p))))))]
      (fn [cont flow]
        (fn [n t]
          (let [o (object-array 9)]
            (doto o
              (aset continuation cont)
              (aset ready true)
              (aset queue 1)
              (aset notifier n)
              (aset terminator t)
              (aset iterator (flow #(more! o) #(doto o (aset continuation nil) (more!))))
              (aset store {})
              (aset result []))
            (more! o)
            (reify
              IFn
              (-invoke [_]
                ;; TODO
                )
              IDeref
              (-deref [_]
                (loop [tail nil
                       live 0]
                  (let [head (aget o queue)
                        live (loop [l live n nil e head]
                               (let [p (u/aget-aset e prev n)]
                                 (if (number? p)
                                   (pull! o (+ l p) e)
                                   (if (identical? tail p)
                                     (pull! o l e) (recur l e p)))))]
                    (if (identical? (aget o queue) head)
                      (do (aset o queue live)
                          (when (zero? live) ((aget o terminator))))
                      (recur head live))))
                (aget o result)))))))))