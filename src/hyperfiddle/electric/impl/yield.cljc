(ns hyperfiddle.electric.impl.yield
  (:require
   [missionary.core :as m]
   [hyperfiddle.rcf :as rcf :refer [tests with tap %]])
  #?(:clj (:import (clojure.lang IFn IDeref))))

;; Superseded by yield2 namespace, which adds work skipping

(defn aswap
  ([^objects arr slot f]            (aset arr slot (f (aget arr slot))))
  ([^objects arr slot f a]          (aset arr slot (f (aget arr slot) a)))
  ([^objects arr slot f a b]        (aset arr slot (f (aget arr slot) a b)))
  ([^objects arr slot f a b c]      (aset arr slot (f (aget arr slot) a b c)))
  ([^objects arr slot f a b c & ds] (aset arr slot (apply f (aget arr slot) a b c ds))))

(def iterator (int 0))
(def busy (int 1))                      ; boolean, true when the process is ready to ready
(def done (int 2))                      ; boolean, true when the main flow (arg 2 to yield, the input) is terminated
(def alive (int 3))                     ; number, count of child processes still alive
(def active (int 4))                    ; state array of the currently focused flow

(declare cancel transfer)
(deftype Yield [check notifier terminator state]
  IFn
  (#?(:clj invoke :cljs -invoke) [this]
    (locking this (cancel state)))
  IDeref
  (#?(:clj deref :cljs -deref) [this]
    (locking this (transfer this))))

(defn ready [^Yield main ^objects child_]
  (let [^objects main_ (.-state main)]
    (loop []
      (when (aset child_ busy (not (aget child_ busy)))
        (if (aget child_ done)
          (do (when (zero? (aswap main_ alive dec))
                ((.-terminator main))) (recur))
          (let [^objects active_ (aget main_ active)]
            (if (identical? child_ active_)
              ((.-notifier main))
              (if (and (identical? child_ main_) (some? active_))
                ;; FIXME don't cancel the child immediately, defer due to work skipping
                (let [it (aget active_ iterator)]
                  (aset main_ active main_) (it)
                  (if (aget active_ busy)
                    (do (try @it (catch #?(:clj Throwable :cljs :default) _))
                        (ready main active_)) ((.-notifier main))))
                (let [it (aget child_ iterator)]
                  (try @it (catch #?(:clj Throwable :cljs :default) _))
                  (recur))))))))))

(defn cancel [^objects s]
  ((aget s iterator)))

(defn transfer [^Yield r]
  (let [^objects s (.-state r)
        ^objects f (aget s active)]
    (try
      (let [x @(aget f iterator)]
        (if (identical? f s)
          ;; TODO if x = current don't run below, just return x
          (if-some [>e ((.-check r) x)]
            (let [f (doto (object-array 3)
                      (aset busy true)
                      (aset done false))
                  i (>e #(locking r (ready r f))
                      #(locking r
                         (aset f done true)
                         (ready r f)))]
              (aset f iterator i)
              (aset s active f)
              (aswap s alive inc)
              (doto (aswap f busy not)
                (assert "Initialization failure."))
              (try @i (finally (ready r f)))) x) x))
      (catch #?(:clj Throwable :cljs :default) e
        (cancel s) (aset s active nil) (throw e))
      (finally (ready r f)))))

(defn yield "
Returns a continuous flow producing values produced by continuous flow >x, passing each sampled value to function f.
If the result is nil, the value is returned as is, otherwise the result is run as a flow and its values are output
until the input flow becomes ready again, at which point the recovery flow is cancelled."
  [f >x]
  (fn [n t]
    (let [main_ (doto (object-array 5)
                  (aset busy true)
                  (aset done false)
                  (aset alive 1))
          r (->Yield f n t main_)]
      (aset main_ active main_)
      (aset main_ iterator
        (>x #(locking r (ready r main_))
          #(locking r
             (aset main_ done true)
             (ready r main_))))
      (doto r (ready main_)))))

(comment
  (def !e (atom "odd"))
  (def !x (atom 0))
  (def it
    ((yield
       (fn [x] (when (odd? x) (m/watch !e)))
       (m/watch !x))
     #(prn :ready) #(prn :done)))
  (it)
  @it := 0
  (swap! !x inc)
  @it := "odd"
  (reset! !e "ODD")
  @it := "ODD"
  (swap! !x inc)
  @it := 2

  )

(tests "work skipping"
  (def !e (atom "odd"))
  (def !x (atom 0))
  (def it
    ((yield
       ;; TODO test m/watch is not cancelled
       (fn [x] (tap x) (when (odd? x) (m/watch !e)))
       (m/watch !x))
     #(do) #(do)))
  @it := 0, % := 0
  (swap! !x inc)
  @it := "odd", % := 1
  (swap! !x identity)
  @it := "odd", % := 1                  ; this 1 shouldn't happen, work skipping
  (it)
  )

(tests "child not cancelled on duplicate"
  (def !x (atom 0))
  (def it
    ((yield
       (fn [x] (tap x) (when (odd? x) (m/observe (fn [!] (! :init) #(tap :cancelled)))))
       (m/watch !x))
     #(do) #(do)))
  @it := 0, % := 0
  (swap! !x inc)
  @it := :init, % := 1
  (swap! !x identity)
  % := :cancelled                       ; wrong
  @it := :init, % := 1
  (it)
  )
