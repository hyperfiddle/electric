(ns ^{:clj-kondo/config '{:lint-as {hyperfiddle.q9.impl/condf clojure.core/condp}}}
    hyperfiddle.q9.impl
  (:refer-clojure :exclude [bound? munge ancestors])
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [hfdl.lang :as p]
   [hyperfiddle.api :as hf] ;; should it be :as-alias?
   [hyperfiddle.spec :as spec]
   [hyperfiddle.walk :as walk]
   [hyperfiddle.rcf :as rcf :refer [tests ! %]]
   )
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

(defn props [form]
  (->> (dissoc (meta form) ::type :line :column)
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))

(defn describe [form]
  (merge {:form form}
         (case (categorize form)
           :symbol {:role  :nav
                    :props (props form)}
           :seq    {:role  :call
                    :props (props form)}
           ;; :vector {:role :collect}
           :map    {:role :traverse}
           nil)))

(defn id-generator [] (let [state (atom -1)] #(swap! state inc)))

(defn meta? [x] (instance? IObj x))

(defn reference? [sym] (and (symbol? sym) (not (meta-keyword? sym))))

(defn args-points [id-gen parent [f & args]]
  (if-let [args-spec (spec/args f)]
    (->> args
         (filter reference?)
         (map-indexed vector)
         (map (fn [[idx arg]]
                {:id     (id-gen)
                 :role   :ref
                 :parent parent
                 :form   arg
                 :spec   (nth args-spec idx)})))
    ()))

(defn meta-points [id-gen parent form]
  (if (meta? form)
    (->> (meta form)
         (mapcat (fn [[k form]] (case (categorize form)
                                  :symbol (cond
                                            (= `hf/as k) [{:id (id-gen) :role :alias :prop k, :form form :parent parent}]
                                            :else        [{:id (id-gen) :role :ref :prop k, :form form :parent parent}] ;; parent is the refered, will be resolved in further pass.
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
                                 (let [k-branch (rec parent k)]
                                   (into k-branch (rec (:id (first k-branch)) v))))
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
                  :ref (if (#{`hf/render} (:prop point))
                         point ;; FIXME skip not because it’s hf/render but because it resolves to the global scope
                         (let [candidates (->> (points-in-scope index point)
                                               (filter (fn [candidate] (and (not= point candidate)
                                                                            (= (:form point) (:form candidate))))))]
                           (case (count candidates)
                             0 (throw (ex-info "Can’t resolve reference" {:ref (:form point)}))
                             1 (assoc point :deps #{(:id (first candidates))})
                             (throw (ex-info "Ambiguous ref" {:ref (:form point)})))))
                  point))))))

;; pass
(defn compute-dependencies [points]
  (let [index    (map-by :id points)
        children (children index)]
    (->> points
         (map (fn [point]
                (let [deps (seq (->> (get children (:id point))
                                     (map index)
                                     (filter (fn [child] (or (#{:ref} (:role child))
                                                             #_(and (some? (:prop child))
                                                                  (not (#{`hf/as} (:prop child)))))))
                                     (map :id)))
                      point (if (contains? point :deps)
                              (update point :deps into deps)
                              (assoc point :deps (set deps)))]
                  (if (or (#{:ref} (:role point))
                          (and (some? (:prop point))
                               (not (#{`hf/as} (:prop point))))
                          (nil? (:parent point)))
                    point
                    (update point :deps conj (:parent point)))))))))

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

(defn identifier [form]
  (condf form
    symbol? form
    seq?    (identifier (first form))))

;; pass
(defn name-points [points]
  (map (fn [point] (assoc point :name (cond-> (str (munge (identifier (:form point))))
                                        (and (:prop point)
                                             (not= `hf/as (:prop point))) (str "_" (munge (name (:prop point))))
                                        (:occurrence point)               (str "_" (:occurrence point))))) points))

(defn rank [index point]
  (if-some [deps (seq (map index (:deps point)))]
    (apply max (map #(inc (rank index %)) deps))
    0))

(defn toposort
  ([points]       (toposort (map-by :id points)))
  ([index points] (sort-by (fn [%] [(rank index %) (:id %)]) points)))

(defn emit-point [index point]
  (case (:role point)
    :nav   [(symbol (:name point)) (let [parent     (get index (:parent point))
                                         parent-sym (if (= :many (:card parent))
                                                      '%
                                                      (list `unquote (symbol (:name parent))))]
                                     `(hf/nav ~parent-sym ~(keyword (:form point))))]
    :call  [(symbol (:name point)) (list 'var (cons `p/$ (:form point)))]
    :alias [(symbol (:name point)) (symbol (:name (get index (:parent point))))]
    :ref   nil))

(defn symbolic [form]
  (condf form
    meta-keyword? (keyword form)
    symbol?       (list 'quote form)
    seq?          (list 'quote form)))

(declare emit)

(defn get-children [index point] (map index (get (children index) (:id point))))

(defn emit-props [index point]
  (->> (get-children index point)
       (filter :prop)
       (map (fn [point] [(keyword (:prop point)) (symbol (:name point))]))
       (reduce conj (:props point))))

(defn reverse-deps [prnts]
  (apply merge-with set/union (for [[parent children] prnts
                                    child             children]
                                {child (if parent #{parent} #{})})))

(defn ancestors [index point]
  (reduce (fn [r id]
            (if-let [parent (get index id)]
              (into r (ancestors index parent))
              r))
          (:deps point) (:deps point)))

(defn scopes [points]
  (let [index (map-by :id points)]
    (group-by (fn [point] (->> (ancestors index point)
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
  (remove #(#{:alias :ref} (:role %)) points))

(defn emit-render [scopes index points]
  (letfn [(rec [point]
            (if (:prop point)
              nil
              (let [nom          (symbol (:name point))
                    attr         (symbolic (:form point))
                    children     (renderables (get-children index point))
                    continuation (if (contains? scopes (:id point))
                                   (emit scopes index (:id point))
                                   (if (seq children)
                                     (into {} (map rec children))
                                     nom))]
                [attr `(var (p/$ render ~(cond
                                           (= :many (:card point)) `(var (p/for [~'% (unquote ~nom)]
                                                                           (var ~continuation)))
                                           (seq children)          `(var ~continuation)
                                           :else                   nom ;; leaf
                                           )
                                 ~(emit-props index point)))])))]
    (into {} (map rec (roots points)))))

(->> '[{(user.gender-shirt-size/submissions "alice")
        [#_:dustingetz/email
         {(props :dustingetz/shirt-size {::hf/options (user.gender-shirt-size/shirt-sizes gender)})
          [:db/ident]}
         {(props :dustingetz/gender {::hf/options (user.gender-shirt-size/genders)
                                     ::hf/render render-options})
          [(props :db/ident {::hf/as gender})]}]}
       #_{(user.gender-shirt-size/genders)
          [:db/ident #_(props :db/ident {::hf/as gender})]}]
     (parse)
     (analyze)
     (emit)
     ;; scopes
     )

(defn emit
  ([points] (let [index (map-by :id points)]
              (emit (scopes points) index nil)))
  ([scopes index id]
   (let [points (toposort index (get scopes id))]
     `(let [~@(mapcat #(emit-point index %) points)]
        (p/$ render (var ~(emit-render scopes index (renderables points)))
             {})))))

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
       parse
       analyze
       ;; toposort
       ;; (remove (fn [point] (or #_(:prop point) (#{:ref} (:role point)))))
       emit
       ))

(defn quoted? [form] (and (seq? form) (= 'quote (first form))))

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

(tests
 (p/run (! (binding [hf/entity 9]
             (hfql [{(user.gender-shirt-size/submissions "alice")
                     [#_:dustingetz/email
                      {(props :dustingetz/shirt-size {::hf/options (user.gender-shirt-size/shirt-sizes gender)})
                       [:db/ident]}
                      {(props :dustingetz/gender {::hf/options (user.gender-shirt-size/genders)
                                                  ::hf/render render-options})
                       [(props :db/ident {::hf/as gender})]}]}
                    #_{(user.gender-shirt-size/genders)
                       [:db/ident #_(props :db/ident {::hf/as gender})]}]) 
             ))) 
 % := _)

;; (rcf/enable! )

;; (alter-var-root #'clojure.pprint/*print-miser-width* (constantly 72))
;; (alter-var-root #'clojure.pprint/*print-right-margin* (constantly 80))
;; (alter-var-root #'clojure.pprint/*print-pprint-dispatch* (constantly clojure.pprint/code-dispatch))


