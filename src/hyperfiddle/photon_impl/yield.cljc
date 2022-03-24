(ns hyperfiddle.photon-impl.yield
  #?(:clj (:import (clojure.lang IFn IDeref))))

(declare cancel sample)
(deftype Yield [check notifier terminator state]
  IFn
  (#?(:clj invoke :cljs -invoke) [this]
    (locking this (cancel state)))
  IDeref
  (#?(:clj deref :cljs -deref) [this]
    (locking this (sample this))))

(defn transfer [^Yield r ^objects f]
  (let [^objects s (.-state r)]
    (loop []
      (when (aset f (int 1) (not (aget f (int 1))))
        (if (aget f (int 2))
          (do (when (zero? (aset s (int 3) (dec (aget s (int 3)))))
                ((.-terminator r))) (recur))
          (let [^objects g (aget s (int 4))]
            (if (identical? f g)
              ((.-notifier r))
              (if (and (identical? f s) (some? g))
                (let [i (aget g (int 0))]
                  (aset s (int 4) s) (i)
                  (if (aget g (int 1))
                    (do (try @i (catch #?(:clj Throwable :cljs :default) _))
                        (transfer r g)) ((.-notifier r))))
                (let [i (aget f (int 0))]
                  (try @i (catch #?(:clj Throwable :cljs :default) _))
                  (recur))))))))))

(defn cancel [^objects s]
  ((aget s (int 0))))

(defn sample [^Yield r]
  (let [^objects s (.-state r)
        ^objects f (aget s (int 4))]
    (try
      (let [x @(aget f (int 0))]
        (if (identical? f s)
          (if-some [>e ((.-check r) x)]
            (let [f (doto (object-array 3)
                      (aset (int 1) true)
                      (aset (int 2) false))
                  i (>e #(locking r (transfer r f))
                      #(locking r
                         (aset f (int 2) true)
                         (transfer r f)))]
              (aset f (int 0) i)
              (aset s (int 4) f)
              (aset s (int 3) (inc (aget s (int 3))))
              (doto (aset f (int 1) (not (aget f (int 1))))
                (assert "Initialization failure."))
              (try @i (finally (transfer r f)))) x) x))
      (catch #?(:clj Throwable :cljs :default) e
        (cancel s) (aset s (int 4) nil) (throw e))
      (finally (transfer r f)))))

(defn yield "
Returns a continuous flow producing values produced by continuous flow >x, passing each sampled value to function f.
If the result is nil, the value is returned as is, otherwise the result is run as a flow and its values are output
until the input flow becomes ready again, at which point the recovery flow is cancelled."
  [f >x]
  (fn [n t]
    (let [s (doto (object-array 5)
              (aset (int 1) true)
              (aset (int 2) false)
              (aset (int 3) 1))
          r (->Yield f n t s)]
      (aset s (int 4) s)
      (aset s (int 0)
        (>x #(locking r (transfer r s))
          #(locking r
             (aset s (int 2) true)
             (transfer r s))))
      (doto r (transfer s)))))

(comment
  (require '[missionary.core :as m])
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