(ns hfdl.impl.switch
  (:require [hfdl.impl.util :as u])
  (:import (java.util.function IntBinaryOperator)
           (java.util.concurrent.atomic AtomicReference AtomicInteger)
           (clojure.lang IDeref IFn)))

(def switch
  (let [iterator (int 0)
        notifier (int 1)
        terminator (int 2)
        transfer (int (bit-shift-left 1 0))
        operational (int (bit-shift-left 1 1))
        toggle (reify IntBinaryOperator (applyAsInt [_ x y] (bit-xor x y)))]
    (letfn [(next! [^AtomicReference sampler ^AtomicInteger control outer it in]
              (when (zero? (bit-and operational (.accumulateAndGet control operational toggle)))
                (if (nil? (u/aget-aset in iterator (u/aget-aset outer iterator it)))
                  (loop []
                    (if-some [x (.get sampler)]
                      (if (.compareAndSet sampler x in)
                        (if (ifn? x)
                          (do ((aget outer notifier)) (x))
                          (let [it (aget x iterator)]
                            (it) (try @it (catch Throwable _))))
                        (recur))
                      (do (.set sampler in) ((aget outer notifier))
                          (more! sampler control outer))))
                  (more! sampler control outer))))
            (more! [^AtomicReference sampler ^AtomicInteger control out]
              (when (zero? (.accumulateAndGet control transfer toggle))
                (if-some [t (aget out terminator)]
                  (loop []
                    (if-some [x (.get sampler)]
                      (do (aset out iterator (if (ifn? x) x (aget x iterator)))
                          (if (.compareAndSet sampler x nil) (do) (recur)))
                      (t)))
                  (let [it (aget out iterator)
                        in (object-array 1)]
                    (aset out iterator
                      (@it
                        #(if-some [it (aget in iterator)]
                           (if (.compareAndSet sampler it in)
                             ((aget out notifier))
                             (if (identical? it (aget out iterator))
                               ((aget out notifier))
                               (try @it (catch Throwable _))))
                           (next! sampler control out it in))
                        #(if-some [it (aget in iterator)]
                           (if (.compareAndSet sampler it nil)
                             (do) (if (identical? it (aget out iterator))
                                    ((aget out terminator))
                                    (more! sampler control out)))
                           (next! sampler control out it (aset in iterator in)))))
                    (next! sampler control out it in)))))]
      (fn [f]
        (fn [n t]
          (let [sampler (AtomicReference.)
                control (AtomicInteger.)
                out (doto (object-array 3) (aset notifier n))
                rdy #(more! sampler control out)]
            (aset out iterator (f rdy #(do (aset out terminator t) (rdy))))
            (rdy)
            (reify
              IFn
              (invoke [_]
                ;; TODO cancellation
                )
              IDeref
              (deref [_]
                ;; TODO catch exceptions
                ;; transfer should happen before cas to prevent post-failure notify
                @(loop []
                   (if-some [in (.get sampler)]
                     (let [it (aget in iterator)]
                       (if (.compareAndSet sampler in it)
                         it (recur)))
                     (aget out iterator)))))))))))

(comment
  (def in1 (atom 0))
  (def in2 (atom :a))
  (def out (atom (m/observe #(def ! %))))
  (def f (switch (m/watch out)))
  (def it (f #(prn :ready) #(prn :done)))

  (reset! out (m/watch in2))
  (reset! in1 1)
  (reset! in2 :b)

  (! :ok)

  @it

  )