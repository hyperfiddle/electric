(ns hfdl.impl.switch
  (:require [hfdl.impl.util :as u])
  #?(:clj (:import (clojure.lang IFn IDeref))))

;; 0: iterator
;; 1: prev in linked list
;; 2: next in linked list
;; 3: currently sampled
;; 4: true if input is ready
;; 5: true if output can be notified
;; 6: count of non-terminated flows

(defn ^:static done! [^objects main terminator]
  (when (zero? (aset main (int 6) (dec (aget main (int 6))))) (terminator)))

(defn ^:static ready! [^objects main notifier]
  (if (aget main (int 5)) (notifier) (aset main (int 5) true)))

(defn ^:static cancel! [^objects main]
  (when-some [item (aget main (int 2))]
    (loop [^objects item item]
      (when-not (identical? item main)
        (let [n (aget item (int 2))]
          (aset item (int 1) nil)
          (aset item (int 2) nil)
          ((aget item (int 0)))
          (recur n))))
    (aset main (int 1) nil)
    (aset main (int 2) nil)
    ((aget main (int 0)))))

(defn ^:static sample! [^objects main notifier]
  (try (aset main (int 5) false)
       (let [^objects item (u/aget-aset main (int 3) nil)
             x @(aget item (int 0))]
         (ready! main notifier) x)
       (catch #?(:clj Throwable
                 :cljs :default) e
         (cancel! main)
         (throw e))))

(deftype It [main notifier terminator]
  IFn
  (#?(:clj invoke :cljs -invoke) [it]
    (locking it (cancel! main)))
  IDeref
  (#?(:clj deref :cljs -deref) [it]
    (locking it (sample! main notifier))))

(defn ^:static transfer! [^It it]
  (let [^objects main (.-main it)]
    (while (aset main (int 4) (not (aget main (int 4))))
      (if-some [^objects prev (aget main (int 1))]
        (let [item (object-array 3)]
          (aset main (int 5) false)
          (aset main (int 6) (inc (aget main (int 6))))
          (aset item (int 1) prev)
          (aset prev (int 2) item)
          (aset main (int 1) item)
          (aset item (int 2) main)
          (let [n #(locking it
                     (if (nil? (aget item (int 1)))
                       (try @(aget item (int 0))
                            (catch #?(:clj Throwable
                                      :cljs :default) _))
                       (loop [^objects head (aget main (int 2))]
                         (if (identical? head item)
                           (do (aset item (int 1) main)
                               (aset main (int 2) item)
                               (if-some [^objects curr (u/aget-aset main (int 3) item)]
                                 (try @(aget curr (int 0))
                                      (catch #?(:clj Throwable
                                                :cljs :default) _))
                                 (ready! main (.-notifier it))))
                           (let [next (aget head (int 2))]
                             (aset head (int 1) nil)
                             (aset head (int 2) nil)
                             ((aget head (int 0)))
                             (recur next))))))
                t #(locking it
                     (when-some [^objects prev (aget item (int 1))]
                       (let [^objects next (aget item (int 2))]
                         (aset next (int 1) prev)
                         (aset prev (int 2) next)
                         (aset item (int 1) nil)
                         (aset item (int 2) nil)))
                     (done! main (.-terminator it)))]
            (aset item (int 0)
              (try (@(aget main (int 0)) n t)
                   (catch #?(:clj Throwable
                             :cljs :default) e
                     (u/failer e n t))))
            (ready! main (.-notifier it))))
        (try @(aget main (int 0))
             (catch #?(:clj Throwable
                       :cljs :default) _))))))

(defn switch "
Given a flow of flows, returns a flow concurrently running the flow with the flows produced by this flow and producing
values produced by the latest nested flow. When a nested flow becomes active, earlier nested flows are cancelled.
" [>>f]
  (fn [n t]
    (let [main (object-array 7)
          it (->It main n t)]
      (aset main (int 1) main)
      (aset main (int 2) main)
      (aset main (int 4) true)
      (aset main (int 5) true)
      (aset main (int 6) 1)
      (locking it
        (aset main (int 0)
          (>>f #(locking it (transfer! it))
            #(locking it (done! main t))))
        (transfer! it) it))))

(comment
  (require '[missionary.core :as m])
  (def !as (vec (repeatedly 10 #(atom 0))))
  (def !a (atom (m/watch (!as 0))))
  (def it ((switch (m/watch !a)) #(prn :ready) #(prn :done)))
  @it
  (swap! (!as 0) inc)
  (reset! !a (m/watch (!as 3)))
  (it)

  )