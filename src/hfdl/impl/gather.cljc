(ns hfdl.impl.gather
  (:require [hfdl.impl.util :as u]
            [missionary.core :as m])
  #?(:clj (:import (clojure.lang IDeref IFn))))

;; 0: iterator
;; 1: prev in the stack of ready to transfer
;; 2: prev in the linked list of cancellable flows
;; 3: next in the linked list of cancellable flows
;; 4: head of the linked list of cancellable flows
;; 5: tail of the linked list of cancellable flows
;; 6: flag that must toggle twice to trigger transfer on input
;; 7: flag that must toggle twice to trigger notify on output
;; 8: count of non-terminated flows

(defn ^:static attach! [^objects main ^objects item]
  (if-some [prev ^objects (aset item (int 2) (aget main (int 5)))]
    (aset prev (int 3) item)
    (aset main (int 4) item))
  (aset main (int 5) item))

(defn ^:static detach! [^objects main ^objects item]
  (let [p (aget item (int 2))
        s (aget item (int 3))]
    (if (nil? p)
      (aset main (int 4) s)
      (aset p (int 3) s))
    (if (nil? s)
      (aset main (int 5) p)
      (aset s (int 2) p))))

(defn ^:static done! [^objects main ^objects item terminator]
  (when-not (identical? item (aget item (int 2))) (detach! main item))
  (when (zero? (aset main (int 8) (dec (aget main (int 8))))) (terminator)))

(defn ^:static ready! [^objects main notifier]
  (when (aset main (int 7) (not (aget main (int 7)))) (notifier)))

(defn ^:static cancel! [main]
  (loop []
    (when-some [item ^objects (aget main (int 4))]
      (aset main (int 4) (aget item (int 3)))
      (aset item (int 3) nil)
      (aset item (int 2) item)
      ((aget item (int 0)))
      (recur))))

(defn ^:static flush! [^objects item]
  (loop [item item]
    (when (some? item)
      (let [p (u/aget-aset item (int 1) nil)]
        (try @(aget item (int 0))
             (catch #?(:clj Throwable :cljs :default) _))
        (recur p)))))

(defn ^:static fail! [^objects main ^objects item error]
  (flush! (u/aget-aset main (int 1) main))
  (flush! item)
  (cancel! main)
  (throw error))

(defn ^:static reduce! [^objects main rf notifier]
  (let [h ^objects (u/aget-aset main (int 1) nil)]
    (loop [p ^objects (u/aget-aset h (int 1) nil)
           r (try @(aget h (int 0))
                  (catch #?(:clj Throwable :cljs :default) e
                    (fail! main p e)))]
      (if (nil? p)
        (do (ready! main notifier) r)
        (let [q (u/aget-aset p (int 1) nil)]
          (recur q
            (try (rf r @(aget p (int 0)))
                 (catch #?(:clj Throwable :cljs :default) e
                   (fail! main q e)))))))))

(deftype It [main rf notifier terminator]
  IFn
  (#?(:clj invoke :cljs -invoke) [it]
    (locking it (cancel! main)))
  IDeref
  (#?(:clj deref :cljs -deref) [it]
    (locking it (reduce! main rf notifier))))

(defn ^:static transfer! [^It it]
  (let [^objects main (.-main it)]
    (while (aset main (int 6) (not (aget main (int 6))))
      (aset main (int 8) (inc (aget main (int 8))))
      (let [item (object-array (int 4))]
        (if (identical? main (aget main (int 2)))
          (aset item (int 2) item)
          (attach! main item))
        (let [n #(locking it
                   (let [p (aget main (int 1))]
                     (if (identical? p main)
                       (try @(aget item (int 0)) (catch #?(:clj Throwable :cljs :default) _))
                       (do (aset main (int 1) item)
                           (when (nil? (aset item (int 1) p))
                             (ready! main (.-notifier it)))))))
              t #(locking it (done! main item (.-terminator it)))
              i (try (@(aget main (int 0)) n t)
                     (catch #?(:clj Throwable :cljs :default) e
                       (u/failer e n t)))]
          (aset item (int 0) i)
          (when (identical? item (aget item (int 2))) (i)))))))

(defn gather "
Given a commutative function and a flow of flows, returns a flow concurrently running the flow with flows produced by
this flow and producing values produced by nested flows, reduced by the function if more than one can be transferred
simultaneously.
" [rf >>x]
  (fn [n t]
    (let [main (object-array (int 9))
          it (->It main rf n t)]
      (doto main
        (aset (int 4) main)
        (aset (int 5) main)
        (aset (int 6) true)
        (aset (int 7) true)
        (aset (int 8) 1))
      (locking it
        (aset main (int 0)
          (>>x #(locking it (transfer! it))
            #(locking it (done! main main t))))
        (transfer! it)
        (ready! main n) it))))

(comment
  (require '[missionary.core :as m])
  (def !xs (repeatedly 5 #(atom 0)))
  (def it ((gather + (m/seed (map m/watch !xs)))
           #(prn :ready) #(prn :done)))
  @it
  (swap! (nth !xs 1) inc)
  (it)

  (def failer (m/ap (throw (ex-info "error" {}))))

  (def it ((gather + (m/seed [(m/watch (nth !xs 0))
                              failer
                              (m/watch (nth !xs 1))
                              (m/observe (fn [!] (def e! !) #(prn :cancelled)))]))
           #(prn :ready) #(prn :done)))
  @it

  (def it ((gather + failer) #(prn :ready) #(prn :done)))
  @it

  )
