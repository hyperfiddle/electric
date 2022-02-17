(ns hyperfiddle.q7-env
  (:refer-clojure :exclude [bound?])
  (:import (clojure.lang Compiler$LocalBinding IObj IMeta IHashEq))
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]
            [clojure.spec.alpha :as s]
            [hyperfiddle.spec :as spec]
            [clojure.set :as set]
            [clojure.string :as str]
            [hyperfiddle.api :as hf]))

(deftype KeywordSym [form]
  Object
  (toString [_this] (str (clojure.core/keyword form)))
  (equals [_this other] (and (instance? KeywordSym other)
                             (= form (.form other))))
  IHashEq
  (hasheq [_this] (hash form))
  IObj
  (withMeta [_this m] (KeywordSym. (with-meta form m)))
  IMeta
  (meta  [_] (meta form)))

(defn keyword-sym? [k] (instance? KeywordSym k))

(defn keyword [named]
  (if (keyword-sym? named)
    (clojure.core/keyword (.form named))
    (clojure.core/keyword named)))

(defn namespace [named] (clojure.core/namespace (if (keyword-sym? named) (.form named) named)))

(defn kw [kw-or-sym]
  (if (keyword-sym? kw-or-sym)
    kw-or-sym
    (KeywordSym. (symbol kw-or-sym))))

(defmethod print-method KeywordSym [sym writer] (.write writer (str "(kw " (keyword sym) ")")))

(defn props? [form]
  (and (seq? form)
       (= '. (second form))
       (even? (count (drop 2 form)))
       (every? keyword-sym? (map first (partition 2 (drop 2 form))))))

(defn parse-props [[_form _dot & props]] (apply hash-map props))

(defn parse [form]
  (clojure.walk/postwalk (fn [form]
                           (cond
                             (keyword? form) (kw form)
                             (props? form)   (vary-meta (first form) merge (parse-props form))
                             :else           form))
                         form))
;; ----

(s/fdef submissions :ret (s/coll-of any?))
(s/fdef genders :ret (s/coll-of any?))
(def test-form (parse '[{(hyperfiddle.q7-env/submissions "") [:user/email
                                                              {(:user/gender . ::hf/options genders) [:db/ident]}
                                                              {(:user/shirt-size . ::hf/options (shirt-sizes user/gender)) [:db/ident]}]}
                        {(hyperfiddle.q7-env/genders) [:db/ident]}]))

;; ----

;; (set! *print-namespace-maps* false)

(def reference? symbol?)

(defn meta? [x] (instance? IObj x))

(defn references
  "Collect all references in `form`."
  [form]
  (letfn [(collect [r form] (cond
                              ;; recursion
                              (and (meta? form) (seq (meta form))) (-> (collect r (with-meta form nil)) (collect (meta form)))
                              (map? form)                          (apply set/union (map (partial collect r) form))
                              (map-entry? form)                    (-> (collect r (key form)) (collect (val form)))
                              (vector? form)                       (apply set/union (map (partial collect r) form))
                              (seq? form)                          (apply set/union (map (partial collect r) (rest form)))
                              ;; collect
                              (reference? form)                    (conj r form)
                              :else                                r))]
    (set (map kw (collect #{} form)))))


(tests
 (references :db/id) := #{}
 (references [:db/id]) := #{}
 (references '[(a b)]) := #{(kw :b)}
 (references '[{:foo/bar [(a b)]}]) := #{(kw :b)}
 (references test-form) := #{(kw :genders) (kw :user/gender)})

(defn scope-boundary?
  "A scope boundary is a point of multiplicity many, inside of which we can’t refer."
  [form]
  (cond
    (seq? form)    (recur (first form))
    (symbol? form) (= ::spec/many (spec/cardinality form))
    :else          false))

(defn- as-set
  ([] #{})
  ([a] (if (set? a) a #{a}))
  ([a & args] (apply set/union a args)))

(defn- as-list
  ([] ())
  ([a] (if (seq? a) a (list a)))
  ([a & args] (concat a (apply concat args))))

(defn declarations
  "Collect all declared points in `form`.
  eg. `:foo` declares `#{:foo}`
      `{:foo [:bar]}` declares `#{:foo :bar}`
      `{(foo bar) [baz]}` declares `#{'(foo bar) baz}` iff `foo` is multiplicity 1. One can’t refer down through multiplicity n."
  ([form] (declarations as-set form))
  ([collectf form]
   (cond
     (keyword-sym? form) (collectf form)
     (keyword? form)     (collectf (kw form))
     (symbol? form)      (collectf (kw form))
     (map? form)         (let [[k v] (first form)]
                           (if (scope-boundary? k)
                             (declarations collectf k)
                             (collectf (declarations collectf k) (declarations collectf v))))
     (vector? form)      (apply collectf (map (partial declarations collectf) form))
     (seq? form)         (collectf (kw (first form)))
     :else               (throw (ex-info "Unknown form" {:form form})))))

(tests
 (declarations :db/id) := #{(kw :db/id)}
 (declarations [:db/id]) := #{(kw :db/id)}
 (declarations '[(a b)]) := #{(kw :a)}
 (declarations [{:user/shirt-size [:db/ident]}]) := #{(kw :user/shirt-size) (kw :db/ident)}
 (declarations [{:user/shirt-size [:db/ident]}
                {:user/gender [:db/ident]}]) := #{(kw :user/shirt-size) (kw :user/gender) (kw :db/ident)}
 (declarations (parse '[{:user/profile [:user/email
                                        {(:user/gender . ::hf/options genders) [:db/ident]}
                                        {(:user/shirt-size . ::hf/options (shirt-sizes user/gender)) [:db/ident]}]}]))
 := #{(kw :user/profile) (kw :user/gender) (kw :user/email) (kw :db/ident) (kw :user/shirt-size)}

 )

(tests
 (s/fdef multiplicity-one :ret any?)
 (s/fdef multiplicity-many :ret (s/coll-of any?))
 (declarations '[{(hyperfiddle.q7-env/multiplicity-many) [:should-not-be-declared]}
                 {(hyperfiddle.q7-env/multiplicity-one) [:should-be-declared]}
                 {:user/gender [:db/ident]}])
 :=
 #{(kw :should-be-declared) (kw 'hyperfiddle.q7-env/multiplicity-one) (kw 'hyperfiddle.q7-env/multiplicity-many) (kw :user/gender) (kw :db/ident)}
 )

(defn- duplicates
  "Return a set of duplicates elements in `coll`."
  [coll]
  (loop [coll       coll
         seen       #{}
         duplicates #{}]
    (if (seq coll)
      (if (contains? seen (first coll))
        (recur (rest coll) seen                     (conj duplicates (first coll)))
        (recur (rest coll) (conj seen (first coll)) duplicates                  ))
      duplicates)))

(tests
 (duplicates []) := #{}
 (duplicates [1 2]) := #{}
 (duplicates [1 1]) := #{1}
 (duplicates [(kw :a) (kw :b) (kw :a)]) := #{(kw :a)})

(defn ambiguous-declarations
  "Collect duplicate declarations in `form`, for which it would be ambiguous to refer to."
  [form] (duplicates (declarations as-list form)))

(tests
 (ambiguous-declarations []) := #{}
 (ambiguous-declarations [{:user/shirt-size [:db/ident1]}
                           {:user/gender [:db/ident2]}])
 := #{}
 (ambiguous-declarations '[(a b)
                           (a b)
                           {:user/shirt-size [:db/ident]}
                           {:user/gender [:db/ident]}])
 := #{(kw 'a) (kw :db/ident)}

 (ambiguous-declarations (parse '[{:user/profile [:user/email
                                                  {(:user/gender . ::hf/options genders) [:db/ident]}
                                                  {(:user/shirt-size . ::hf/options (shirt-sizes user/gender)) [:db/ident]}]}]))
 )

(defn- cons' [x seq]
  (when-not (nil? seq)
    (cons x seq)))

;; (contains? (ambiguous-declarations tree) ref) (throw (ex-info "Ambiguous reference" {:reference ref}))
;; (not (contains? (declarations tree) ref)) (throw (ex-info "Undeclared reference" {:declared (declarations tree), :reference ref}))

(defn paths [tree ref]
  (letfn [(rec [r tree] (cond
                          (= tree ref)   (if (seq r) (list r) r)
                          (vector? tree) (seq (remove nil? (mapcat #(rec (cons' tree r) %) tree)))
                          (map? tree)    (seq (remove nil? (mapcat #(rec (cons' tree r) %) (vals tree))))))]
    (set (rec () tree))))

(tests (paths (kw :db/id) (kw :db/id)) := #{}
       (paths (parse [:db/id]) (kw :db/id)) := #{(list (parse [:db/id]))}
       (let [tree (parse '[{:user/profile [:user/email
                                           {(:user/gender . ::hf/options genders) [:db/id :db/ident]}
                                           {(:user/shirt-size . ::hf/options (shirt-sizes user/gender)) [:db/ident]}]}])]
         (paths tree (kw :db/id))
         := #{(list [(kw :db/id) (kw :db/ident)]
                    {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                    [(kw :user/email)
                     {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                     {(kw :user/shirt-size) [(kw :db/ident)]}]
                    {(kw :user/profile)
                     [(kw :user/email)
                      {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                      {(kw :user/shirt-size) [(kw :db/ident)]}]}
                    [{(kw :user/profile)
                      [(kw :user/email)
                       {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                       {(kw :user/shirt-size) [(kw :db/ident)]}]}])}

         (paths tree (kw :db/ident))
         := #{(list [(kw :db/ident)]
                    {(kw :user/shirt-size) [(kw :db/ident)]}
                    [(kw :user/email)
                     {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                     {(kw :user/shirt-size) [(kw :db/ident)]}]
                    {(kw :user/profile)
                     [(kw :user/email)
                      {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                      {(kw :user/shirt-size) [(kw :db/ident)]}]}
                    [{(kw :user/profile)
                      [(kw :user/email)
                       {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                       {(kw :user/shirt-size) [(kw :db/ident)]}]}])
              (list [(kw :db/id) (kw :db/ident)]
                    {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                    [(kw :user/email)
                     {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                     {(kw :user/shirt-size) [(kw :db/ident)]}]
                    {(kw :user/profile)
                     [(kw :user/email)
                      {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                      {(kw :user/shirt-size) [(kw :db/ident)]}]}
                    [{(kw :user/profile)
                      [(kw :user/email)
                       {(kw :user/gender) [(kw :db/id) (kw :db/ident)]}
                       {(kw :user/shirt-size) [(kw :db/ident)]}]}])}))

;; (defn find-parent [tree decl]
;;   (cond
;;         :else
;;     (let [found (first (remove #{decl} (filter (partial tree-seq-contains? decl) (reverse (tree-seq coll? identity tree)))))]
;;       (if (map-entry? found)
;;         {(key found) (val found)}
;;         found))))
;;
;; (tests
;;  (find-parent (kw :db/id) (kw :db/id)) := nil #_(kw :db/id)
;;  (find-parent [(kw :db/id)] (kw :db/id)) := [(kw :db/id)]
;;  (find-parent (parse {:user/shirt-size [:db/id]}) (kw :db/id)) := [(kw :db/id)]
;;  (find-parent (parse '[(user/shirt-sizes db/ident)
;;                        {:user/gender [:db/ident]}]) (kw :db/ident)) := [(kw :db/ident)]
;;
;;  (let [tree (parse '[{:user/profile [:user/email
;;                                      {(:user/gender . ::hf/options genders) [:db/id]}
;;                                      {(:user/shirt-size . ::hf/options (shirt-sizes user/gender)) [:db/ident]}]}])]
;;    (find-parent tree (kw :db/id)) := [(kw :db/id)]
;;    (find-parent tree (find-parent tree (kw :db/id))) := [(kw :db/id)]
;;    )
;;  )

(defn- conj' [set v]
  (if (nil? v)
    set
    (conj set v)))

(defn- add-dep [env k v]
  (if (contains? env k)
    (update env k conj' v)
    (assoc env k (if (nil? v) #{} #{v}))))

;; (defn- refs-deps [form]
;;   (loop [refs (references form)
;;          r    {}]
;;     (if-some [[decl & refs] (seq refs)]
;;       (if (contains? (declarations form) decl)
;;         (let [r (loop [parent (find-parent form decl)
;;                        r      r]
;;                   (cond
;;                     (= parent form)  r
;;                     (vector? parent) (recur (find-parent form parent) r) ;; skip intermediate steps like [:db/id]
;;                     :else            (recur (find-parent form parent) (add-dep r decl parent))))]
;;           (recur refs (add-dep r (find-parent form (symbol decl)) decl)))
;;         (recur refs r))
;;       r)))

;; (defn dependencies
;;   "Compute dependencies between referred points in `form`.
;;   eg. {a #{}, b #{a}}"
;;   [form]
;;   (let [env (refs-deps form)]
;;     ;; add top levels
;;     (reduce (fn [r dep] (if (contains? r dep) r (add-dep r dep nil)))
;;             env (apply set/union (vals env)))))

(defn dependencies [form]
  (let [decls (declarations form)]
    (reduce (fn [r ref]
              (if (contains? decls ref)
                (assoc r ref (paths form ref))
                r)) {} (references form))))

(tests
 (dependencies :db/id)                        := {}
 (dependencies :user/gender)                  := {}
 (dependencies '(user/shirt-sizes gender))    := {}
 (dependencies '[(user/shirt-sizes user/gender)
                 {:user/gender [:db/ident]}]) := '{(user/shirt-sizes user/gender) #{:user/gender},
                                                   :user/gender                   #{{:user/gender [:db/ident]}},
                                                   {:user/gender [:db/ident]}     #{}}
 (dependencies '[(user/shirt-sizes db/ident)
                 {:user/gender [:db/id :db/ident]}])
 := '{(user/shirt-sizes db/ident)       #{:db/ident},
      :db/ident                         #{{:user/gender [:db/id :db/ident]}},
      {:user/gender [:db/id :db/ident]} #{}}

 (s/fdef submissions :ret any?)
 (dependencies '[{(hyperfiddle.q7-env/submissions "") [:user/email
                                                       {:user/gender [:db/ident (genders)]}
                                                       {:user/shirt-size [:db/ident (shirt-sizes user/gender)]}]}
                 {(hyperfiddle.q7-env/genders) [:db/ident]}])
 )

(defn rank
  "Compute the rank of dependencies."
  [deps form]
  (let [parents (get deps form)]
    (if (= #{} parents)
      0
      (apply max (map (fn [parent] (inc (rank deps parent))) parents)))))

(tests
 (let [deps (dependencies '[(user/shirt-sizes db/ident)
                            {:user/gender [:db/id :db/ident]}])]
   (rank deps {:user/gender [:db/id :db/ident]}) := 0
   (rank deps :db/ident) := 1
   (rank deps '(user/shirt-sizes db/ident)) := 2))

(defn toposort-dependencies
  "Sort dependencies by rank."
  [deps]
  (sort-by (fn [[k _v]] (rank deps k)) deps))

(tests
 (toposort-dependencies (dependencies '[(user/shirt-sizes db/ident)
                                        {:user/gender [:db/id :db/ident]}]))
 := '([{:user/gender [:db/id :db/ident]} #{}]
      [:db/ident #{{:user/gender [:db/id :db/ident]}}]
      [(user/shirt-sizes db/ident) #{:db/ident}]))

(defn leaves
  "Nodes without dependents"
  [deps]
  (let [paths (apply set/union (vals deps))]
    (reduce-kv (fn [r k v]
                 (if (contains? paths k)
                   r
                   (conj r k)))
               #{} deps)))

(tests
 (leaves (dependencies '[{(user/shirt-sizes db/ident) [:db/id]}
                         {:user/gender [:db/id :db/ident]}]))
 := #{'(user/shirt-sizes db/ident)})

(defn eligible-bindings
  [deps]
  (toposort-dependencies (apply dissoc deps (leaves deps))))
