(ns hyperfiddle.incseq.items-eager-impl
  (:require [contrib.data :refer [->box]]
            [hyperfiddle.electric.impl.array-fields :as a]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p])
  (:import #?(:clj [clojure.lang IDeref IFn])
           [missionary Cancelled]))

(def ps-field-count (a/deffields -stepped -cancelled -go -input-ps -done -diff -item*))

(declare cleanup-then-done)
(defn call [f] (f))
(deftype Ps [step done state-]
  IFn (#?(:clj invoke :cljs -invoke) [_]
        (some-> (a/get state- -input-ps) call)
        (a/ncas state- -done ::yes ::requested)
        (let [cancelled? (a/getset state- -cancelled true)]
          (when (not (or (a/getset state- -stepped true) cancelled?)) (step))))
  IDeref (#?(:clj deref :cljs -deref) [this]
           (a/set state- -stepped false)
           (when (identical? ::requested (a/get state- -done))  (cleanup-then-done this))
           (let [diff (a/getset state- -diff nil)]
             (if (a/get state- -cancelled) (throw (Cancelled.)) diff))))
(defn cleanup-then-done [^Ps ps]
  (a/fset ps -input-ps nil, -done ::yes, -item* nil)
  ((.-done ps)))

(def item-field-count (a/deffields -v -flow -ps* -dead))
(deftype Item [state-])

(def item-ps-field-count (a/deffields -stepped -cancelled -cache))

(defn remove-item-ps [^Item item ps] (let [ps* (a/fget item -ps*)] (ps* (disj (ps*) ps))))

(defn cleanup-item-ps [ps a done] (when-not (identical? ps (a/getset a -cache ps))  (done)))

(defn ->item-ps [^Item item step done]
  (let [a (object-array item-ps-field-count)]
    (a/set a -cache a, -cancelled false)
    (reify
      IFn
      (#?(:clj invoke :cljs -invoke) [this]
        (remove-item-ps item this)
        (let [cancelled? (a/getset a -cancelled true)]
          (when (not (or (a/getset a -stepped true) cancelled?)) (step))))
      (#?(:clj invoke :cljs -invoke) [_ v]
        (when-not (or (= v (a/getset a -cache v)) (a/getset a -stepped true))
          (step)))
      IDeref
      (#?(:clj deref :cljs -deref) [this]
        (a/set a -stepped false)
        (if (a/get a -cancelled)
          (do (cleanup-item-ps this a done) (throw (Cancelled.)))
          (a/get a -cache))))))

(let [cancelled #?(:clj (Object.) :cljs (js/Object.))]
  (defn ->dead-item-ps [step done -v]
    (step)
    (let [<s> (->box -v)]
      (reify
        IFn (#?(:clj invoke :cljs -invoke) [_] (<s> cancelled))
        IDeref (#?(:clj deref :cljs -deref) [this]
                 (done)
                 (if (identical? cancelled (<s>))  (throw (Cancelled.))  (let [v (<s>)] (<s> this) v)))))))

(defn grow! [^Ps ps {d :degree, n :grow}]
  (run! (fn [i]
          (let [^Item item (->Item (object-array item-field-count))]
            (a/fset item -ps* (->box #{}))
            (a/set (a/fget ps -item*) i item)
            (a/fswap ps -diff update :change assoc i
                     (a/fset item -flow (fn [step done]
                                          (if (a/fget item -dead)
                                            (->dead-item-ps step done (a/fget item -v))
                                            (let [item-ps (->item-ps item step done), ps* (a/fget item -ps*)]
                                              (ps* (conj (ps*) item-ps))
                                              (item-ps (a/fget item -v))
                                              item-ps)))))))
    (range (- d n) d)))

(defn permute! [^Ps ps {p :permutation}]
  (let [rot* (p/decompose conj #{} p)
        item* (a/fget ps -item*)]
    (run! (fn [rot] (apply a/rot item* rot)) rot*)))

(defn shrink! [^Ps ps {d :degree, n :shrink}]
  (let [item* (a/fget ps -item*)]
    (run! (fn [i]
            (let [^Item item (a/get item* i)]
              (a/fset item -dead true)
              (run! #(%) ((a/fget item -ps*)))))
      (range (- d n) d))))

(defn change! [^Ps ps diff]
  (let [item* (a/fget ps -item*)]
    (reduce-kv (fn [_ i v]
                 (let [^Item item (a/get item* i)]
                   (a/fset item -v v)
                   (run! (fn [item-ps] (item-ps v)) ((a/fget item -ps*)))))
      nil (:change diff))))

(defn needed-diff? [d]
  (or (seq (:permutation d)) (pos? (:grow d)) (pos? (:shrink d)) (seq (:freeze d))))

(defn transfer-input [^Ps ps]
  (loop [diff (a/fgetset ps -diff {:change {}})]
    (a/fset ps -go true)
    (let [in-diff @(a/fget ps -input-ps)]
      (grow! ps in-diff)
      (permute! ps in-diff)
      (shrink! ps in-diff)
      (change! ps in-diff)
      (let [newdiff (a/fset ps -diff (cond->> (assoc in-diff :change (:change (a/fget ps -diff)))
                                       diff (d/combine diff)))]
        (if (a/fgetset ps -go false)
          (case (a/fget ps -stepped)
            false (when (needed-diff? newdiff) (a/fset ps -stepped true) ((.-step ps)))
            true nil
            nil (do (a/fset ps -stepped true) ((.-step ps))))
          (recur newdiff))))))

(defn flow [input]
  (fn [step done]
    (let [ps (->Ps step done (object-array ps-field-count))]
      (a/fset ps -item* (object-array 8), -stepped nil, -go true, -done ::no)
      (a/fset ps -input-ps (input
                             #(when-not (a/fgetset ps -go false) (transfer-input ps))
                             #(if (or (a/fget ps -stepped) (a/fget ps -go))
                                (a/fset ps -done ::requested)
                                (cleanup-then-done ps))))
      (transfer-input ps) ps)))
