(ns hyperfiddle.incseq.items-eager-impl
  (:require [hyperfiddle.incseq.diff-impl :as d])
  #?(:clj (:import [clojure.lang IFn IDeref])))

(defn flow [incseq]
  (fn [step done]
    (let [input-ps (incseq #() #())
          input-diff @input-ps]
      (step)
      (reify
        IFn (#?(:clj invoke :cljs -invoke) [_] (input-ps) (done))
        IDeref (#?(:clj deref :cljs -deref) [_] input-diff))
      )))
