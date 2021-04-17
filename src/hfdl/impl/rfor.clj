(ns hfdl.impl.rfor
  (:require [hfdl.impl.util :as u])
  (:import (clojure.lang IFn IDeref)
           (java.util.concurrent.atomic AtomicReference)))

;; TODO error handling
(def spawn
  (let [iterator (int 0)
        previous (int 1)
        position (int 2)
        branches (int 2)
        sampling (int 3)
        inner-length (int 3)
        outer-length (int 4)
        mark! (fn [^AtomicReference state item n]
                (loop []
                  (let [q (.get state)]
                    (aset item previous (case q nil 0 q))
                    (if (.compareAndSet state q item)
                      (when (number? q) (n)) (recur)))))
        done! (fn [^AtomicReference state item t]
                (aset item iterator nil)
                (loop []
                  (let [q (.get state)]
                    (aset item previous (case q nil 0 q))
                    (if (number? q)
                      (if (.compareAndSet state q (dec q))
                        (case q 1 (t) nil) (recur))
                      (if (.compareAndSet state q item)
                        (do) (recur))))))
        kill! (fn [item]
                (when-some [it (aget item iterator)] (it)))
        tear! (fn [store id item]
                (aset item position nil)
                (kill! item) (dissoc store id))]
    (fn [f in n t]
      (let [state (AtomicReference. 1)
            outer (object-array outer-length)
            token (->> (in #(mark! state outer n) #(done! state outer t))
                    (aset outer iterator) (u/token))]
        (reify
          IFn
          (invoke [_]
            (u/burn-token! token))
          IDeref
          (deref [_]
            (loop [live 0]
              (let [live
                    (loop [live live
                           head (.getAndSet state nil)]
                      (let [prev (aget head previous)
                            _ (aset head previous nil)
                            curr (aget outer sampling)
                            live (if-some [it (aget head iterator)]
                                   (if (identical? outer head)
                                     (do (aset outer sampling [])
                                         (let [diff
                                               (reduce
                                                 (fn [diff id]
                                                   (let [b (aget outer branches)
                                                         s (aget outer sampling)
                                                         p (count s)]
                                                     (if-some [inner (get b id)]
                                                       (do (aset outer sampling
                                                             (conj s (nth curr (aget inner position))))
                                                           (aset inner position p)
                                                           (dissoc diff id))
                                                       (let [inner (object-array inner-length)
                                                             it ((f id)
                                                                 #(if-some [it (aget inner iterator)]
                                                                    (if (nil? (aget inner position))
                                                                      (try @it (catch Throwable _))
                                                                      (mark! state inner n))
                                                                    (aset inner iterator inner))
                                                                 #(done! state inner t))]
                                                         (when (nil? (aget inner iterator))
                                                           (prn "TODO reactive-for failure : inner flow has no initial value."))
                                                         (aset inner position p)
                                                         (aset inner iterator it)
                                                         (aset outer branches (assoc b id inner))
                                                         (aset outer sampling (conj s @it)) diff))))
                                                 (aget outer branches) @it)
                                               union (aget outer branches)]
                                           (->> (reduce-kv tear! union diff)
                                             (aset outer branches)
                                             (vals)
                                             (cons outer)
                                             (partial run! kill!)
                                             (u/swap-token! token))
                                           (+ live (- (count union) (count curr)))))
                                     (do
                                       (if-some [pos (aget head position)]
                                         (aset outer sampling (assoc curr pos @it))
                                         (try @it (catch Throwable _))) live))
                                   (dec live))]
                        (if (number? prev) (+ live prev) (recur live prev))))]
                (if (.compareAndSet state nil live)
                  (case live 0 (t) nil) (recur live))))
            (aget outer sampling)))))))