(ns hyperfiddle.q7-env2
  (:refer-clojure :exclude [bound?])
  (:import (clojure.lang IObj IMeta IHashEq))
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [hyperfiddle.spec :as spec]
            [dorothy.core :as dot]
            [dorothy.jvm :as djvm]))

(defn meta-keyword? [k] (= ::MetaKeyword (type k)))

(defn kw [named]
  (if (meta-keyword? named)
    named
    (vary-meta (symbol named) assoc :type ::MetaKeyword)))

(defn props? [form]
  (and (seq? form)
       (= '. (second form))
       (even? (count (drop 2 form)))
       (every? meta-keyword? (map first (partition 2 (drop 2 form))))))

(defn parse-props [[_form _dot & props]] (apply hash-map props))

(defn parse [form]
  (walk/postwalk (fn [form]
                   (cond
                     (keyword? form) (kw form)
                     (props? form)   (vary-meta (first form) merge (parse-props form))
                     :else           form))
                 form))

(defn categorize [node]
  (condp #(%1 %2) node
    symbol?       :symbol
    seq?          :seq
    vector?       :vector
    map?          :map))

(defn node-point [node]
  (case (categorize node)
    :symbol      node
    :seq         (recur (first node))
    :vector      nil ;; Can’t refer to edges form
    :map         (recur (ffirst node))))

(defn node-hash [node]
  (case (categorize node)
    (:symbol :seq) (hash [node (meta node)])
    :vector        (hash (map node-hash node))
    :map           (hash [(node-hash (key (first node))) (node-hash (key (first node)))])))

(defn meta? [x] (instance? IObj x))

(defn node-name [node] (some-> (node-point node) (str/replace  #"\/" "_")))

(defn identifier [node]
  (when-let [id (if (and (meta? node) (get (meta node) (kw ::hf/as)))
                  (get (meta node) (kw ::hf/as))
                  (case (categorize node)
                    :symbol (symbol (str "nav__" (node-name node) "_" (node-hash node)))
                    :seq    (symbol (str "call__" (node-name node) "_" (node-hash node)))
                    :vector (symbol (str "collect__" (node-hash node)))
                    :map    (symbol (str "traverse__" (node-name node) "_" (node-hash (key (first node))) "->" (node-hash (val (first node)))))))]
    (vary-meta id assoc ::node node)))

(defn reference? [sym] (and (symbol? sym) (not (meta-keyword? sym))))

(defn references [form]
  (letfn [(collect [r form] (cond
                              ;; recursion
                              (map? form)       (apply set/union (map (partial collect r) form))
                              (map-entry? form) (-> (collect r (key form)) (collect (val form)))
                              (vector? form)    (apply set/union (map (partial collect r) form))
                              (seq? form)       (apply set/union (map (partial collect r) (rest form)))
                              ;; collect
                              (reference? form) (conj r form)
                              :else             r))]
    (if (and (meta? form) (seq (meta form)))
      (if (meta-keyword? form)
        (collect #{} (dissoc (meta form) `hf/as))
        (-> (collect #{} (with-meta form nil))
            (collect (dissoc (meta form) `hf/as))))
      (case (categorize form)
        :seq    (apply set/union (map (partial collect #{}) (rest form)))
        :symbol (collect #{} (meta form))
        :map    (references (ffirst form))
        #{}))))

(defn children [node]
  (case (categorize node)
    :vector node
    :map    (children (val (first node)))
    nil))

(defn dependencies
  ([node] (dependencies {} node))
  ([r node]
   (if (nil? node)
     r
     (let [children (children node)
           r        (assoc r (identifier node) (set (mapv identifier children)))]
       (cond
         (= node children)  (reduce dependencies r children)
         (vector? children) (reduce dependencies (assoc r (identifier node) #{(identifier children)}
                                                        (identifier children) (set (mapv identifier children))) children)
         :else              (reduce dependencies r children))))))

(defn reverse-deps [deps]
  (let [points (apply set/union (set (keys deps)) (vals deps))
        r      (zipmap points (repeat (count points) #{}))]
    (reduce-kv (fn [r dep deps]
                 (reduce (fn [r' dep'] (update r' dep' conj dep))
                         r deps))
               r deps)))

(defn ancestors [deps⁻¹ point]
  (let [parents (seq (get deps⁻¹ point))]
    (apply set/union parents (map (partial ancestors deps⁻¹) parents))))

(defn card-n? [node]
  (when-let [point (node-point (::node (meta node)))]
    (= ::spec/many (spec/cardinality point))))

(defn crossed-card-n-boundary
  "Returns a set of card-n nodes, `a` have to cross to refer to `b`. Which is forbidden."
  [deps referencing-dep refered-dep]
  (let [deps⁻¹             (reverse-deps deps)
        card-n-ancestors-a (set (filter card-n? (ancestors deps⁻¹ referencing-dep)))
        card-n-ancestors-b (set (filter card-n? (ancestors deps⁻¹ refered-dep)))]
    (set/difference (set/union card-n-ancestors-a card-n-ancestors-b)
                    (set/intersection card-n-ancestors-a card-n-ancestors-b))))

(defn resolve-ref [deps ref]
  (if (contains? deps ref)
    (key (find deps ref)) ;; symbol in deps has extra meta
    (first (filter #(= ref (node-point (::node (meta %)))) (keys deps)))))

(defn check-card-n-crossed-boundaries! [deps node]
  (let [refs    (references (::node (meta node)))
        crosses (->> refs
                     (map (fn [ref] (resolve-ref deps ref)))
                     (filter (fn [dep] (symbol? (node-point (::node (meta dep))))))
                     (map (fn [dep] (crossed-card-n-boundary deps node dep)))
                     (apply set/union))]
    (when (seq crosses)
      (throw (ex-info "Can't reference through cardinality many"
                      {:references         refs
                       :referencing-node   (::node (meta node))
                       :crossed-boundaries (set (map (comp ::node meta) crosses))})))))

(defn add-ref-deps [deps]
  (reduce-kv (fn [r k v]
               (if-some [refs (seq (references (::node (meta k))))]
                 (do
                   (check-card-n-crossed-boundaries! deps k)
                   (assoc r k (into v (map (partial resolve-ref deps) refs))))
                 r))
             deps deps))

(defn rank [deps point]
  (let [children (get deps point)]
    (if (= #{} children)
      0
      (apply max (map #(inc (rank deps %)) children)))))

(defn toposort [deps]
  (reduce-kv (fn [r k _v] (assoc r k (rank deps k)))
             {} deps))

;; --------------------------------------------------

(s/fdef submissions :ret (s/coll-of any?))
(s/fdef genders :ret (s/coll-of any?))
(def test-form '[{(hyperfiddle.q7-env/submissions "") [:user/email
                                                       {(:user/gender . ::hf/options hyperfiddle.q7-env/genders) [(:db/ident . ::hf/as gender)]}
                                                       {(:user/shirt-size . ::hf/options (shirt-sizes gender)) [:db/ident]}]}
                 {(hyperfiddle.q7-env/genders) [:db/ident]}])

(tests
 (let [deps (dependencies (parse test-form))]
   (reverse-deps (reverse-deps deps)) := deps))

(tests
 (references (parse '(:user/gender . ::hf/options genders))) := #{'genders}
 (references (parse '{(:user/shirt-size . ::hf/options (shirt-sizes gender user/gender)) [:db/id]})) := #{'gender 'user/gender}
 )

(tests (dependencies (parse test-form))
       := '{gender                                                        #{},
            traverse__hyperfiddle.q7-env_submissions_887674486->813813191 #{collect__813813191},
            collect__1456979475                                           #{nav__db_ident_-2025222904},
            traverse__hyperfiddle.q7-env_genders_-1268712499->1456979475  #{collect__1456979475},
            traverse__user_shirt-size_-532734933->1456979475              #{collect__1456979475},
            traverse__user_gender_-1713793610->585698254                  #{collect__585698254},
            nav__user_email_1246913086                                    #{},
            collect__813813191                                            #{traverse__user_shirt-size_-532734933->1456979475
                                                                            traverse__user_gender_-1713793610->585698254
                                                                            nav__user_email_1246913086},
            collect__-816940683                                           #{traverse__hyperfiddle.q7-env_submissions_887674486->813813191
                                                                            traverse__hyperfiddle.q7-env_genders_-1268712499->1456979475},
            nav__db_ident_-2025222904                                     #{},
            collect__585698254                                            #{gender}})

(tests
 (toposort (dependencies (parse test-form))) := '{gender                                                        0,
                                                  traverse__hyperfiddle.q7-env_submissions_887674486->813813191 4,
                                                  collect__1456979475                                           1,
                                                  traverse__hyperfiddle.q7-env_genders_-1268712499->1456979475  2,
                                                  traverse__user_shirt-size_-532734933->1456979475              2,
                                                  traverse__user_gender_-1713793610->585698254                  2,
                                                  nav__user_email_1246913086                                    0,
                                                  collect__813813191                                            3,
                                                  collect__-816940683                                           5,
                                                  nav__db_ident_-2025222904                                     0,
                                                  collect__585698254                                            1}
 )

(comment
  (defn viz!* [deps]
    (let [nodes (mapv (fn [dep] [(keyword dep) {:label  (pr-str (::node (meta dep)))
                                                :xlabel (str (`hf/as (meta (::node (meta dep)))))
                                                :shape  "none"}]) (keys deps))
          links (mapcat (fn [[dep deps]] (mapv (fn [child] [(keyword dep) (keyword child)]) deps)) deps)]
      (->> (concat nodes links)
           (dot/graph)
           (dot/dot)
           (djvm/show!))))

  (defn viz! [form] (viz!* (add-ref-deps (dependencies (parse form)))))

  (viz! test-form)

  (viz!* (reverse-deps (add-ref-deps (dependencies (parse test-form))))))

