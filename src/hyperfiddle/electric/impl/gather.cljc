(ns hyperfiddle.electric.impl.gather
  (:require [hyperfiddle.electric.impl.failer :as failer])
  #?(:clj (:import (clojure.lang IDeref IFn)))
  #?(:cljs (:require-macros [hyperfiddle.electric.impl.gather :refer [aget-aset]])))

#?(:clj
   (defmacro aget-aset [arr idx val]
     `(let [a# ~arr
            i# ~idx
            x# (aget a# i#)]
        (aset a# i# ~val) x#)))

;; 0: iterator
;; 1: prev in linked list
;; 2: next in linked list
;; 3: next in transfer stack
;; 4: true if input is ready
;; 5: true if output can be notified
;; 6: count of non-terminated flows

(defn ^:static done! [^objects main terminator]
  (when (zero? (aset main (int 6) (dec (aget main (int 6))))) (terminator)))

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

(defn ^:static flush! [item]
  (loop [^objects item item]
    (when (some? item)
      (let [next (aget-aset item (int 3) nil)]
        (try @(aget item (int 0))
             (catch #?(:clj Throwable :cljs :default) _))
        (recur next)))))

(defn ^:static fail! [^objects main ^objects item error]
  (cancel! main)
  (flush! (aget-aset main (int 3) nil))
  (flush! item)
  (throw error))

(defn ^:static sample! [^objects main rf notifier]
  (let [^boolean idle (aget-aset main (int 5) false)
        ^objects head (aget-aset main (int 3) nil)]
    (loop [^objects item (aget-aset head (int 3) nil)
           r (try @(aget head (int 0))
                  (catch #?(:clj Throwable :cljs :default) e
                    (fail! main item e)))]
      (if (nil? item)
        (do (if (aget main (int 5))
              (when idle (notifier))
              (aset main (int 5) ^Object idle)
              ;; ^ Reflection optimization: no static method RT.aset
              ;; for (^objects ^int ^boolean), target (^objects ^int ^object)
              ;; instead.
              )
            r)
        (let [next (aget-aset item (int 3) nil)]
          (recur next
            (try (rf r @(aget item (int 0)))
                 (catch #?(:clj Throwable :cljs :default) e
                   (fail! main next e)))))))))

(deftype It [main rf notifier terminator]
  IFn
  (#?(:clj invoke :cljs -invoke) [it]
    (locking it (cancel! main)))
  IDeref
  (#?(:clj deref :cljs -deref) [it]
    (locking it (sample! main rf notifier))))

(defn ^:static transfer! [^It it]
  (let [^objects main (.-main it)]
    (while (aset main (int 4) (not (aget main (int 4))))
      (if-some [^objects prev (aget main (int 1))]
        (let [item (object-array (int 4))
              ^boolean idle (aget-aset main (int 5) false)]
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
                       (if-some [^objects curr (aget-aset main (int 3) item)]
                         (aset item (int 3) curr)
                         (if (aget main (int 5))
                           ((.-notifier it))
                           (aset main (int 5) true)))))
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
                   (catch #?(:clj Throwable :cljs :default) e
                     (failer/run e n t))))
            (if (aget main (int 5))
              (when idle ((.-notifier it)))
              (aset main (int 5) idle))))
        (try @(aget main (int 0))
             (catch #?(:clj Throwable
                       :cljs :default) _))))))

(defn gather "
Given a commutative function and a flow of flows, returns a flow concurrently running the flow with flows produced by
this flow and producing values produced by nested flows, reduced by the function if more than one can be transferred
simultaneously.
" [rf >>x]
  (fn [n t]
    (let [main (object-array (int 7))
          it (->It main rf n t)]
      (doto main
        (aset (int 1) main)
        (aset (int 2) main)
        (aset (int 4) true)
        (aset (int 5) true)
        (aset (int 6) 1))
      (locking it
        (aset main (int 0)
          (>>x #(locking it (transfer! it))
            #(locking it (done! main t))))
        (transfer! it) it))))

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
