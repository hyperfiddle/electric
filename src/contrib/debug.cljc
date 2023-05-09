(ns contrib.debug
  #?(:cljs (:require-macros contrib.debug))
  (:require [clojure.string :as str]
            [hyperfiddle.electric :as e])
  (:import #?(:clj [clojure.lang IFn IDeref])
           [hyperfiddle.electric Failure]))

(defmacro dbg
  ([form] `(dbg '~form ~form))
  ([label form]
   (let [[label form] (if (keyword? form) [form label] [label form])]
     `(let [v# ~form] (prn ~label '~'==> v#) v#))))

(defmacro dbgv [form]
  `(let [args# [~@form], v# ~form] (prn '~form '~'==> (cons '~(first form) (rest args#))  '~'==> v#) v#))

(defmacro dbgc [[op & args :as form]]
  `(let [op# ~op, args# ~args, ret# (apply op# args#)]
     (prn '~form)
     (doseq [[form# arg#] (map vector '~args args#)]
       (prn '~'_ form# '~'==> arg#))
     (prn '~'==> ret#)
     ret#))

(defmacro do-traced [& body] `(do ~@(for [form body] `(dbg ~form))))

(def !id (atom 0))

(defn instrument* [nm flow]
  (fn [n t]
    (let [id (swap! !id inc)
          it (flow #(do (prn nm id :notified) (n)) #(do (prn nm id :terminated) (t)))]
      (reify
        IFn (#?(:clj invoke :cljs -invoke) [_] (prn nm id :cancelled) (it))
        IDeref (#?(:clj deref :cljs -deref) [_]
                 (let [v @it]
                   (prn nm id :transferred
                     (if (instance? Failure v)
                       (let [e (.-error v)]
                         [(type e) (ex-message e)])
                       v))
                   v))))))
(defmacro instrument [nm & body] `(new (instrument* ~nm (e/fn [] ~@body))))
