(ns hyperfiddle.incseq.items-eager-impl
  (:require [hyperfiddle.electric.impl.array-fields :as a])
  #?(:clj (:import [clojure.lang IDeref IFn])))

(def ps-field-count (a/deffields input-ps input-stepper input-doner diff v* flow*))
(deftype Ps [step done state-]
  IFn (#?(:clj invoke :cljs -invoke) [_] ((a/get state- input-ps)) (done))
  IDeref (#?(:clj deref :cljs -deref) [_] (a/get state- diff)))

(defn transfer-input [^Ps ps]
  (let [diff @(a/fget ps input-ps)]
    (reduce (fn [diff k]
              (let [v (get (:change diff) k) flow* (a/fget ps flow*) v* (a/fget ps v*)]
                (a/set v* k v)
                (->> (a/set flow* k (fn [step done]
                                      (step)
                                      (reify
                                        IFn (#?(:clj invoke :cljs -invoke) [_] (done))
                                        IDeref (#?(:clj deref :cljs -deref) [_] (a/get v* k)))))
                  (update diff :change assoc k))))
      diff (range (- (:degree diff) (:grow diff)) (:degree diff)))))
(defn consume-input-step [^Ps ps] (fn [] (a/fset ps diff (transfer-input ps)) ((.-step ps))))
(defn consume-input-done [^Ps ps] (fn []))

(defn flow [input]
  (fn [step done]
    (let [ps (->Ps step done (object-array ps-field-count))]
      (a/fset ps input-stepper #() input-doner #(), flow* (object-array 8), v* (object-array 8))
      (a/fset ps input-ps (input (fn [] ((a/fget ps input-stepper))) (fn [] ((a/fget ps input-doner)))))
      (a/fset ps input-stepper (consume-input-step ps), input-doner (consume-input-done ps))
      (a/fset ps diff (transfer-input ps)) (step) ps)))
