(ns hyperfiddle.incseq.items-eager-impl
  (:require [contrib.data :refer [->box]]
            [contrib.debug :as dbg]
            [clojure.set :as set]
            [hyperfiddle.electric.impl.array-fields :as a]
            [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p])
  (:import #?(:clj [clojure.lang IDeref IFn])
           #?(:clj [java.util.concurrent.atomic AtomicLong])
           #?(:clj [java.util.concurrent.locks ReentrantLock])
           [missionary Cancelled])
  #?(:cljs (:require-macros [hyperfiddle.incseq.items-eager-impl :refer [locked]])))

#?(:clj (set! *warn-on-reflection* true))

(def ps-field-count (a/deffields -stepped -cancelled -input-ps -diff -item*))

(defmacro locked [on & body]
  (if (:js-globals &env)
    `(do ~@body)
    (let [l (with-meta (gensym "lock") {:tag `ReentrantLock})]
      `(let [~l ~on] (.lock ~l) (let [v# (do ~@body)] (.unlock ~l) v#)))))

(declare cleanup-then-done)
(defn call [f] (f))
(deftype Ps [step done going indone state- #?(:clj lock)]
  IFn (#?(:clj invoke :cljs -invoke) [this]
        (let [step? (locked (.-lock this)
                      (swap! indone (fn [v] (if (= v ::yes) ::yes ::requested)))
                      (let [cancelled? (a/getset state- -cancelled true)]
                        (not (or (a/getset state- -stepped true) cancelled? (= ::yes @indone)))))]
          (some-> (a/get state- -input-ps) call)
          (when step? (step))))
  IDeref (#?(:clj deref :cljs -deref) [this]
           (let [[cleanup? ?diff] (locked (.-lock this)
                                    (a/set state- -stepped false)
                                    [(= ::requested @indone) (a/getset state- -diff nil)])]
             (when cleanup? (cleanup-then-done this))
             (cond (a/get state- -cancelled) (throw (Cancelled.))
                   (map? ?diff) ?diff
                   :else (throw ?diff)))))

(defn cleanup-then-done [^Ps ps]
  (locked (.-lock ps) (a/fset ps -input-ps nil, -item* nil))
  (reset! (.-indone ps) ::yes)
  ((.-done ps)))
(defn going [^Ps ps] #?(:clj (let [^AtomicLong i (.-going ps)] (.longValue i))
                        :cljs (.-going ps)))
(defn ++going [^Ps ps] #?(:clj (let [^AtomicLong i (.-going ps)] (.incrementAndGet i))
                          :cljs (set! (.-going ps) (inc (.-going ps)))))
(defn --going [^Ps ps] #?(:clj (let [^AtomicLong i (.-going ps)] (.decrementAndGet i))
                          :cljs (set! (.-going ps) (dec (.-going ps)))))

(def item-field-count (a/deffields -v -flow -ps* -dead))
(deftype Item [state-])

(def item-ps-field-count (a/deffields _stepped _cancelled -cache -orphaned)) ; -stepped would warn of redefinition

(defn remove-item-ps [^Item item ps] (swap! (a/fget item -ps*) disj ps))

(defn cleanup-item-ps [ps a done] (when-not (= ps (a/getset a -cache ps))  (done)))

(defprotocol Orphanable (orphan [_]))

(defn ->item-ps [^Item item step done]
  (let [a (object-array item-ps-field-count)]
    (a/set a -cache a, -cancelled false, -orphaned false)
    (reify
      IFn
      (#?(:clj invoke :cljs -invoke) [this]
        (remove-item-ps item this)
        (let [cancelled? (a/getset a -cancelled true)]
          (when (not (or (a/getset a -stepped true) cancelled?)) (step))))
      (#?(:clj invoke :cljs -invoke) [_ v]
        (when-not (or (= v (a/getset a -cache v)) (a/getset a -stepped true))
          (step)))
      Orphanable (orphan [this]
                   (a/set a -orphaned true)
                   (remove-item-ps item this)
                   (when-not (a/get a -stepped) (cleanup-item-ps this a done)))
      IDeref
      (#?(:clj deref :cljs -deref) [this]
        (a/set a -stepped false)
        (let [v (a/get a -cache)]
          (when (a/get a -orphaned) (cleanup-item-ps this a done))
          (if (a/get a -cancelled)
            (do (cleanup-item-ps this a done) (throw (Cancelled.)))
            v))))))

(let [cancelled #?(:clj (Object.) :cljs (js/Object.))]
  (defn ->dead-item-ps [step done -v]
    (step)
    (let [<s> (->box -v)]
      (reify
        IFn (#?(:clj invoke :cljs -invoke) [_] (<s> cancelled))
        Orphanable (orphan [_])
        IDeref (#?(:clj deref :cljs -deref) [this]
                 (done)
                 (if (identical? cancelled (<s>))  (throw (Cancelled.))  (let [v (<s>)] (<s> this) v)))))))

(defn grow! [^Ps ps {d :degree, n :grow, p :permutation}]
  (let [idx (set/map-invert p)]
    (a/fgetset ps -item* (a/ensure-fits (a/fget ps -item*) d))
    (run! (fn [i]
            (let [^Item item (->Item (object-array item-field-count))]
              (a/fset item -ps* (atom #{}))
              (a/set (a/fget ps -item*) i item)
              (a/fswap ps -diff update :change assoc (idx i i)
                       (a/fset item -flow (fn [step done]
                                            (if (a/fget item -dead)
                                              (->dead-item-ps step done (a/fget item -v))
                                              (let [item-ps (->item-ps item step done)]
                                                (swap! (a/fget item -ps*) conj item-ps)
                                                (item-ps (a/fget item -v))
                                                item-ps)))))))
      (range (- d n) d))))

(defn permute! [^Ps ps {p :permutation}]
  (let [rot* (p/decompose conj #{} p)
        item* (a/fget ps -item*)]
    (run! (fn [rot] (apply a/rot item* rot)) rot*)))

(defn shrink! [^Ps ps {d :degree, n :shrink}]
  (let [item* (a/fget ps -item*)]
    (run! (fn [i]
            (let [^Item item (a/get item* i)]
              (a/fset item -dead true)
              (a/set item* i nil)
              (run! orphan @(a/fget item -ps*))))
      (range (- d n) d))))

(defn change! [^Ps ps diff]
  (let [item* (a/fget ps -item*)]
    (reduce-kv (fn [_ i v]
                 (let [^Item item (a/get item* i)]
                   (a/fset item -v v)
                   (run! (fn [item-ps] (item-ps v)) @(a/fget item -ps*))))
      nil (:change diff))))

(defn needed-diff? [d]
  (or (seq (:permutation d)) (pos? (:grow d)) (pos? (:shrink d)) (seq (:freeze d))))

(defn transfer-input [^Ps ps]
  (let [step?
        (locked (.-lock ps)
          (loop [diff (a/fgetset ps -diff {:change {}})]
            (let [?in-diff (try @(a/fget ps -input-ps) (catch #?(:clj Throwable :cljs :default) e e))]
              (if (map? ?in-diff)
                (do (grow! ps ?in-diff) (permute! ps ?in-diff) (shrink! ps ?in-diff) (change! ps ?in-diff)
                    (let [newdiff (a/fset ps -diff (cond->> (assoc ?in-diff :change (:change (a/fget ps -diff)))
                                                     diff (d/combine diff)))]
                      (if (= 1 (going ps))
                        (case (a/fget ps -stepped)
                          false (when (needed-diff? newdiff) (a/fset ps -stepped true))
                          true nil
                          nil (a/fset ps -stepped true))
                        (do (--going ps) (recur newdiff)))))
                (do (some-> (a/fget ps -input-ps) call)
                    (swap! (.-indone ps) (fn [v] (if (= ::yes v) ::yes ::requested)))
                    (a/fset ps -diff (if (zero? (going ps)) (ex-info "uninitialized input process" {}) ?in-diff))
                    (not (a/fgetset ps -stepped true)))))))]
    (--going ps)
    (when step? ((.-step ps)))))

(def +initial-item-size+ 8)
(defn flow [input]
  (fn [step done]
    (let [^Ps ps (->Ps step done #?(:clj (new AtomicLong -1) :cljs -1) (atom ::no) (object-array ps-field-count)
                   #?(:clj (new ReentrantLock)))]
      (a/fset ps -item* (object-array +initial-item-size+), -stepped nil)
      (a/fset ps -input-ps (input
                             #(when (= 1 (++going ps)) (transfer-input ps))
                             #(if (or (pos? (going ps)) (a/fget ps -stepped))
                                (reset! (.-indone ps) ::requested)
                                (cleanup-then-done ps))))
      (++going ps) (transfer-input ps) ps)))
