(ns hyperfiddle.incseq.items-eager-impl
  (:require [contrib.data :refer [->box]]
            [hyperfiddle.electric.impl.array-fields :as a]
            [hyperfiddle.incseq.perm-impl :as p])
  #?(:clj (:import [clojure.lang IDeref IFn])))

(def ps-field-count (a/deffields -input-ps -input-stepper -input-doner -diff -item*))
(deftype Ps [step done state-]
  IFn (#?(:clj invoke :cljs -invoke) [_] ((a/get state- -input-ps)) (done))
  IDeref (#?(:clj deref :cljs -deref) [_] (a/get state- -diff)))

(def item-field-count (a/deffields -v -flow -ps*))
(deftype Item [state-])

(def item-ps-field-count (a/deffields -step -done -cache))
(let [nul #?(:clj (Object.) :cljs (js/Object.))]
  (defn ->item-ps [^Item item step done]
    (let [a (object-array item-ps-field-count)]
      (a/set a -cache nul)
      (letfn [(step-idle [] (a/set a -step step-loaded) (step))
              (step-loaded [])]
        (a/set a -step step-idle)
        (reify
          IFn (#?(:clj invoke :cljs -invoke) [this] (let [ps* (a/fget item -ps*)] (ps* (disj (ps*) this))) (done))
          (#?(:clj invoke :cljs -invoke) [_ v] (when (not= v (a/get a -cache)) (a/set a -cache v) ((a/get a -step))))
          IDeref (#?(:clj deref :cljs -deref) [_] (a/set a -step step-idle) (a/get a -cache)))))))

(defn grow! [^Ps ps diff]
  (run! (fn [i]
          (let [^Item item (->Item (object-array item-field-count))]
            (a/fset item -ps* (->box #{}))
            (a/set (a/fget ps -item*) i item)
            (a/fswap ps -diff update :change assoc i
                     (a/fset item -flow (fn [step done]
                                          (let [item-ps (->item-ps item step done), ps* (a/fget item -ps*)]
                                            (ps* (conj (ps*) item-ps))
                                            (item-ps (a/fget item -v))
                                            item-ps))))))
    (range (- (:degree diff) (:grow diff)) (:degree diff))))

(defn permute! [^Ps ps {p :permutation}]
  (let [rot* (p/decompose conj #{} p)
        item* (a/fget ps -item*)]
    (run! (fn [rot] (apply a/rot item* rot)) rot*)))

(defn ->item ^Item [^Ps ps i] (a/get (a/fget ps -item*) i))

(defn change! [^Ps ps diff]
  (reduce-kv (fn [_ i v]
               (let [^Item item (->item ps i)]
                 (a/fset item -v v)
                 (run! (fn [item-ps] (item-ps v)) ((a/fget item -ps*)))))
    nil (:change diff)))

(defn transfer-input [^Ps ps]
  (let [diff @(a/fget ps -input-ps)]
    (a/fset ps -diff {:change {}})
    (grow! ps diff)
    (permute! ps diff)
    (change! ps diff)
    (dissoc diff :change)))

(defn needed-diff? [d]
  (or (seq (:permutation d)) (pos? (:grow d)) (pos? (:shrink d)) (seq (:freeze d))))

(defn consume-input-step [^Ps ps]
  (fn [] (when (needed-diff? (a/fswap ps -diff merge (transfer-input ps)))  ((.-step ps)))))
(defn consume-input-done [^Ps ps] (fn []))

(defn flow [input]
  (fn [step done]
    (let [ps (->Ps step done (object-array ps-field-count))]
      (a/fset ps -input-stepper #() -input-doner #(), -item* (object-array 8))
      (a/fset ps -input-ps (input (fn [] ((a/fget ps -input-stepper))) (fn [] ((a/fget ps -input-doner)))))
      (a/fset ps -input-stepper (consume-input-step ps), -input-doner (consume-input-done ps))
      (a/fswap ps -diff merge (transfer-input ps)) (step) ps)))
