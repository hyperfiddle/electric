(ns ^{:clj-kondo/config '{:lint-as {hyperfiddle.q9.impl/condf clojure.core/condp}}}
    hyperfiddle.q9.impl
  (:refer-clojure :exclude [bound? munge ancestors])
  (:require
   [clojure.string :as str]
   [hfdl.lang :as p]
   [hyperfiddle.api :as hf] ;; should it be :as-alias?
   [hyperfiddle.q9.env :as env]
   [hyperfiddle.rcf :as rcf :refer [! % tests]]
   [hyperfiddle.spec :as spec]
   [hyperfiddle.walk :as walk]
   [missionary.core :as m])
  (:import
   (clojure.lang IObj)))

;; syntax https://gist.github.com/dustingetz/20fbb48773130cf4f91c9d0fc373a737

(defmacro ^{:style/indent 1} condf [expr & clauses] `(condp #(%1 %2) ~expr ~@clauses))

(defn meta-keyword? [sym] (= ::MetaKeyword (::type (meta sym))))

(defn meta-keyword
  ([named] (meta-keyword named nil))
  ([named metas] (if (meta-keyword? named) named (vary-meta (symbol named) merge metas {::type ::MetaKeyword}))))

(defn props? [form]
  (and (seq? form)
       (= 3 (count form))
       (= 'props (first form))
       (map? (last form))))

(defn parse-props [[_props form metas]] (vary-meta form merge metas))

(defn parse-call [[f & args :as sexpr]]
  (if (meta-keyword? f)
    f
    sexpr))

(defn parse [form]
  (walk/postwalk (fn [form]
                   (condf form
                     keyword? (meta-keyword form)
                     props?   (parse-props form)
                     seq?     (parse-call form)
                     form))
                 form))

(defn categorize [form]
  (condf form
    symbol?  :symbol
    seq?     :seq
    vector?  :vector
    map?     :map
    nil))

(defn get-props [form]
  (->> (dissoc (meta form) ::type :line :column :bound)
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))

(defn describe [form]
  (merge {:form form}
         (case (categorize form)
           :symbol {:role  :nav
                    :props (get-props form)}
           :seq    {:role  :call
                    :props (get-props form)}
           ;; :vector {:role :collect}
           :map    {:role :traverse}
           nil)))

(defn id-generator [] (let [state (atom -1)] #(swap! state inc)))

(defn meta? [x] (instance? IObj x))

(defn reference? [sym] (and (symbol? sym) (not (meta-keyword? sym))
                            #_(not (:bound (meta sym)))))

(defn quoted? [form] (and (seq? form) (= 'quote (first form))))

(defn args-points [id-gen parent [f & args]]
  (if-let [args-spec (spec/args f)]
    (->> args
         (map-indexed (fn [idx form]
                        (let [id   (id-gen)
                              refs (->> form
                                        (tree-seq (every-pred coll? (complement quoted?)) identity)
                                        (filter reference?)
                                        (map (fn [sym]
                                               {:id     (id-gen)
                                                :role   (if (= '. sym) :input :ref)
                                                :parent id
                                                :form   sym})))]
                          (cons {:id       id
                                 :role     :arg
                                 :parent   parent
                                 :form     form
                                 :position idx
                                 :arg-name (:name (nth args-spec idx))}
                                refs))))
         (mapcat identity))
    ()))

(defn meta-points [id-gen parent form]
  (if (meta? form)
    (->> (meta form)
         (mapcat (fn [[k form]] (case (categorize form)
                                  :symbol (cond
                                            (= `hf/as k) [{:id (id-gen) :role :alias :prop k, :form form :parent parent}]
                                            #_           (reference? form)
                                            :else        [{:id (id-gen) :role :ref :prop k, :form form :parent parent}] ;; ref target will be resolved later.
                                            )
                                  :seq    (let [id (id-gen)]
                                            (vec (concat [{:id id :role :call :prop k, :parent parent, :form form}]
                                                         (args-points id-gen id form))))
                                  nil)))
         (filter identity))
    ()))

(defn points
  ([form] (points (id-generator) nil form))
  ([id-gen parent form]
   (letfn [(rec [parent form]
             (case (categorize form)
               :symbol (let [id    (id-gen)
                             sym   (merge {:id id :parent parent} (describe form))
                             metas (meta-points id-gen id form)]
                         (vec (cons sym metas)))
               :seq    (let [id    (id-gen)
                             call  (merge {:id id :parent parent} (describe form))
                             args  (args-points id-gen id form)
                             metas (meta-points id-gen id form)]
                         (vec (cons call (concat args metas))))
               :vector (mapcat (partial rec parent) form)
               :map    (mapcat (fn [[k v]]
                                 (let [k-branch  (rec parent k)
                                       parent-id (:id (first k-branch))
                                       options   (first (filter (comp #{`hf/options} :prop) k-branch))]
                                   (cond-> (into k-branch (rec parent-id v))
                                     (some? options) (into (remove :prop (rec (:id options) v))))))
                               form)))]
     (rec parent form))))

;; pass
(defn add-cardinality [point]
  (case (:role point)
    :call (let [[f & _] (:form point)]
            (assoc point :card (case (spec/cardinality f)
                                 nil         nil
                                 ::spec/many :many
                                 ::spec/one  :one)))
    :prop (case  (categorize (:form point))
            :call (add-cardinality (assoc point :role :call))
            point)
    point))

(defn children [index]
  (reduce-kv (fn [r id point]
               (update r (:parent point) (fnil conj #{}) id))
             {} index))

(defn lineage [index point]
  (when-let [parent (get index (:parent point))]
    (cons parent (lineage index parent))))

(defn points-in-scope [index point]
  (let [children            (children index)
        crossable-boundary? (set (filter #(= :many (:card %)) (lineage index point)))]
    (letfn [(descendants [point]
              (if (and (= :many (:card point))
                       (not (crossable-boundary? point)))
                (list point)
                (->> (get children (:id point))
                     (map index)
                     (mapcat descendants)
                     (cons point))))]
      (mapcat descendants (map index (get children nil))))))

;; pass
(defn map-by [kf maps] (into {} (map (juxt kf identity)) maps))

(defn resolve-refs [points]
  (let [index (map-by :id points)]
    (->> points
         (map (fn [point]
                (case (:role point)
                  :ref (if (reference? (:form point))
                         (let [candidates (->> (points-in-scope index point)
                                               (filter (fn [candidate] (and (not= point candidate)
                                                                            (not (#{:ref :arg} (:role candidate)))
                                                                            (= (:form point) (:form candidate))))))]
                           (case (count candidates)
                             0 (if (:external (meta (:form point)))
                                 point
                                 (throw (ex-info "Can’t resolve reference" {:ref (:form point)})))
                             1 (assoc point :deps #{(:id (first candidates))})
                             (throw (ex-info "Ambiguous ref" {:ref (:form point)}))))
                         point)
                  point))))))

(defn get-children [index point] (map index (get (children index) (:id point))))

(defn rank [index point]
  (if-some [deps (seq (map index (:deps point)))]
    (apply max (map #(inc (rank index %)) deps))
    0))

;; pass
(defn compute-dependencies [points]
  (let [index    (map-by :id points)
        children (children index)]
    (->> points
         (map (fn [point]
                (let [deps  (seq (->> (get children (:id point))
                                      (map index)
                                      (filter #(#{:ref :arg :input} (:role %)))
                                      (map :id)))
                      point (update point :deps (fnil into #{}) deps)]
                  (cond (#{:ref :arg} (:role point)) point
                        (#{:input} (:role point))    (let [[_arg call parent & _] (lineage index point)]
                                                       (if (or (= :many (:card parent))
                                                               (= :many (:card call)))
                                                         (update point :deps conj (:id parent))
                                                         point))
                        (nil? (:parent point))       point
                        (some? (:prop point))
                        (cond
                          (= `hf/as (:prop point)) (update point :deps conj (:parent point))
                          :else                    point)
                        :else                        (update point :deps conj (:parent point)))))))))

(defn down-scope
  ([index point] (down-scope index point true))
  ([index point force]
   (let [children (children index)]
     (if (and (not force) (= :many (:card point)))
       nil ;; FIXME invert polarity?
       (->> (get children (:id point))
            (map index)
            (mapcat #(down-scope index % false))
            (cons point))))))

;; pass
(defn unique-name-points [points]
  (let [index  (map-by :id points)
        scopes (filter #(= :many (:card %)) points)]
    (reduce (fn [r scope]
              (->> (down-scope index scope true)
                   (group-by :form)
                   (mapcat (fn [[_form points]]
                             (case (count (remove (comp #{:ref} :role) points))
                               (0 1) points
                               (map-indexed (fn [idx point] (assoc point :occurrence idx)) points))))
                   (concat r)
                   ))
            () scopes))) ;; FIXME root points not scanned if not card :many

(defn munge [point] (str/replace (str point) #"[\.\/]" "_"))

(defn identifier [point]
  (case (:role point)
    :arg (str "arg_" (:parent point) "_" (:position point))
    (letfn [(rec [form]
              (condf form
                symbol? form
                seq?    (rec (first form))))]
      (rec (:form point)))))

;; pass
(defn name-points [points]
  (let [index (map-by :id points)]
    (map (fn rec [point] (assoc point :name (case (:role point)
                                              :arg   (symbol (str (:name (rec (get index (:parent point))))
                                                                  "_"
                                                                  (name (:arg-name point))))
                                              :input (symbol (str (:name (rec (get index (:parent point))))
                                                                  "_"
                                                                  "input"))
                                              (symbol (cond-> (str (munge (identifier point)))
                                                        (and (:prop point)
                                                             (#{`hf/options} (:prop point))) (str "_" (munge (name (:prop point))))
                                                        (:occurrence point)                  (str "_" (:occurrence point))))))) points)))

(defn replace* "Recursive `clojure.core/replace`"
  [smap coll]
  (if (empty? smap) coll (walk/prewalk (fn [form]
                                         (if (quoted? form)
                                           (reduced form)
                                           (if-let [subst (get smap form)]
                                             (reduced subst)
                                             form)))
                                       coll)))

(defn emit-point [index point]
  (let [nom (:name point)]
    (case (:role point)
      :nav   [nom (let [parent     (get index (:parent point))
                        parent-sym (if (= :many (:card parent))
                                     '%
                                     (list `unquote (:name parent)))]
                    `(hf/nav ~parent-sym ~(keyword (:form point))))]
      :call  [nom (let [[f & _args] (:form point)]
                    (list 'var (cons `p/$ (cons f (->> (map index (:deps point))
                                                       (filter #(= :arg (:role %)))
                                                       (sort-by :position)
                                                       (map (fn [point] `(unquote ~(:name point)))))))))]
      :arg   [nom (let [refs (into {} (->> (map index (:deps point))
                                           (remove #(:external (meta (:form %))))
                                           (map (fn [point] [(:form point) (if (= :input (:role point))
                                                                             `(unquote (m/watch ~(:name point)))
                                                                             `(unquote ~(:name point)))]))))]
                    (list 'var (replace* refs (:form point))))]
      :input [nom `(atom nil)]
      :alias [nom (:name (get index (:parent point)))]
      :ref   nil)))

(defn ancestors [index point]
  (reduce (fn [r id]
            (if-let [parent (get index id)]
              (into r (ancestors index parent))
              r))
          (:deps point) (:deps point)))

(defn scopes [points]
  (let [index (map-by :id points)]
    (group-by (fn [point]
                (->> (ancestors index point)
                     (map index)
                     (sort-by #(rank index %))
                     (filter #(= :many (:card %)))
                     (first)
                     (:id)))
              points)))

(defn roots [points]
  (let [present? (set (map :id points))]
    (remove #(present? (:parent %)) points)))

(defn renderables [points]
  (remove #(#{:alias :ref :arg :input} (:role %)) points))

(declare emit emit-render)

(def ^:dynamic *scopes*)
(def ^:dynamic *index*)
(def ^:dynamic *props?* false)

(defn var-point [form] (if *props?* form `(var ~form)))
(defn render-point [form props] (if *props?* form `(p/$ render ~form ~props)))

(defn symbolic-key [point]
  (let [form (or (:prop point) (:form point))]
    (condf form
      meta-keyword? (keyword form)
      symbol?       (list 'quote form)
      seq?          (list 'quote form))))

(declare emit*)

(defn emit-1 [point]
  (binding [*props?* (or *props?* (some? (:prop point)))]
    (let [nom      (:name point)
          attr     (symbolic-key point)
          children (renderables (get-children *index* point))
          cont     (if (contains? *scopes* (:id point))
                     (emit* (:id point))
                     (if (seq children)
                       (into {} (map emit-1 (remove :prop children)))
                       nom))]
      (if *props?*
        [attr (cond
                (= :many (:card point)) `(p/for [~'% (unquote ~nom)]
                                           ~cont)
                (seq children)          cont
                :else                   (if (and (:prop point) (not (#{`hf/options} (:prop point))))
                                          (:form point)
                                          `(unquote ~nom)))]
        (let [props (some->> (get-children *index* point)
                             (filter :prop)
                             (map emit-1)
                             (map (fn [[k v]] [k (if (#{::hf/options} k) `(var ~v) v)]))
                             (seq)
                             (into {}))]
          [attr (var-point (render-point (cond
                                           (= :many (:card point)) `(var (p/for [~'% (unquote ~nom)]
                                                                           (var ~cont)))
                                           (seq children)          `(var ~cont)
                                           :else                   nom)
                                         props))])))))

(defn emit-render [points] (into {} (map emit-1 (remove :prop (roots points)))))

(defn toposort
  ([points]       (toposort (map-by :id points)))
  ([index points] (sort-by (fn [%] [(rank index %) (:id %)]) points)))

(defn emit* [point-id]
  (let [points (toposort *index* (get *scopes* point-id))]
    `(let [~@(mapcat #(emit-point *index* %) points)]
       ~(render-point (var-point (emit-render (renderables points))) nil))))

(defn emit [points]
  (let [index (map-by :id points)]
    (binding [*scopes* (scopes points)
              *index*  index]
      (emit* nil))))

(defn analyze [form]
  (->> (points form)
       (map add-cardinality)
       resolve-refs
       compute-dependencies
       unique-name-points
       name-points
       ))

(defmacro hfql [form]
  (->> form
       (env/resolve-syms (env/make-env &env))
       parse
       analyze
       ;; toposort
       ;; (remove (fn [point] (or #_(:prop point) (#{:ref} (:role point)))))
       emit
       ))

(p/defn join-all [v]
  (cond
    (quoted? v) v
    (map? v)    (into {} (p/for [[k v] v] [k ~v]))
    (list? v)   (seq (p/for [v v] ~v))
    (coll? v)   (into (empty v) (p/for [v v] ~v))
    :else       v))

(p/defn render [>v props]
  (if-let [renderer (::hf/render props)]
    (p/$ renderer >v props)
    (p/$ join-all ~>v)))

(p/defn render-options [>v props]
  (into [:select {:value (p/$ render >v (dissoc props ::hf/render))}]
        (p/for [opt ~(::hf/options props)]
          [:option opt])))

;; DONE
(tests
 (p/run (! (binding [hf/entity 9]
             (hfql [{(user.gender-shirt-size/submissions .)
                     [#_:dustingetz/email
                      ;; (user.gender-shirt-size/shirt-sizes . .)
                      {(props :dustingetz/shirt-size {::hf/options (user.gender-shirt-size/shirt-sizes gender .)
                                                      ;; ::hf/render render-options
                                                      })
                       [:db/id]}
                      {(props :dustingetz/gender {::hf/options (user.gender-shirt-size/genders)
                                                  ::hf/render render-options
                                                  })
                       [(props :db/ident {::hf/as gender})]}]}
                    #_{(user.gender-shirt-size/genders)
                       [:db/ident #_(props :db/ident {::hf/as gender})]}]) 
             ))) 
 % := _)










(comment
  (tests
   ;; Call inner fn with incorrect arity, %2 arg binding (b) leaks from parent fn
   (p/run (! (p/$ (p/fn [a b] (p/$ (p/fn [c d] [c d]) 3))
                  1 2)))
   % := _ ;; FAIL Should throw, instead return [3 2]
   ))

;; (rcf/enable! )

;; (alter-var-root #'clojure.pprint/*print-miser-width* (constantly 72))
;; (alter-var-root #'clojure.pprint/*print-right-margin* (constantly 80))
;; (alter-var-root #'clojure.pprint/*print-pprint-dispatch* (constantly clojure.pprint/code-dispatch))



