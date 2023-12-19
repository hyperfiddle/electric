(ns hyperfiddle.electric.impl.lang-de
  (:require [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.incseq :as i]))

(def ^:dynamic *tier*)

(defn invoke [f & args]
  (apply f args))

(defmacro defs [& exprs]
  `(fn [tier# id#]
     (binding [*tier* tier#]
       (case id#
         ~(interleave (range) exprs)))))

(defmacro static [expr]
  `(r/pure ~expr))

(defmacro ap [& args]
  `(i/latest-product invoke ~@args))

(defmacro free [id]
  `(r/ctor-free (r/tier-ctor *tier*) ~id))

(defmacro local [id]
  `(r/tier-local *tier* ~id))

(defmacro remote [id]
  `(r/tier-local *tier* ~id))

(defmacro ctor [slots output & free]
  `(r/pure (r/peer-ctor (r/tier-peer *tier*) ~slots ~output
             (doto (object-array ~(count free))
               ~@(map-indexed (partial list `aset) free)))))

(defmacro call [id]
  `(i/latest-concat (r/tier-slot *tier* ~id)))

(defmacro join [expr]
  `(i/latest-concat ~expr))

(defmacro var [id]
  `(r/pure (r/peer-var (r/tier-peer *tier*) (quote ~id))))