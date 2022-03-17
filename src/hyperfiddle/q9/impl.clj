(ns ^{:clj-kondo/config '{:lint-as {hyperfiddle.q9.impl/condf clojure.core/condp}}}
    hyperfiddle.q9.impl
  (:refer-clojure :exclude [bound? munge ancestors])
  (:require
   [clojure.string :as str]
   [hfdl.lang :as p]
   [hyperfiddle.api :as hf] ;; should it be :as-alias?
   [hyperfiddle.q9.env :as env]
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

(defn parse-call [[f & _args :as sexpr]]
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
           :map    {:role :traverse}
           nil)))

(defn id-generator [] (let [state (atom -1)] #(swap! state inc)))

(defn meta? [x] (instance? IObj x))

(defn reference? [sym] (and (symbol? sym) (not (meta-keyword? sym))
                            (not= '% sym)
                            #_(not (:bound (meta sym)))))

(defn quoted? [form] (and (seq? form) (= 'quote (first form))))

(defn args-points [id-gen parent [f & args]]
  (if (#{`p/fn} f)
    ()
    (let [args-spec (spec/args f)]
      (cond

        (nil? args-spec)
        (throw (ex-info (str "Couldn’t find `:args` spec for `" f "`") {}))

        (not= (count args) (count args-spec))
        (throw (ex-info (str "`" f "` called with wrong number of arguments. Spec says " (mapv :name args-spec), ", but " (vec args) " was provided")
                        {}))
        :else
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
                                     :arg-name (:name (nth args-spec idx))
                                     :props    (get-props form)}
                                    refs))))
             (mapcat identity))))))

(defn meta-points [id-gen parent form]
  (if (meta? form)
    (->> (meta form)
         (mapcat (fn rec [[k form]]
                   (case (categorize form)
                     :symbol (cond
                               (= `hf/as k)   [{:id (id-gen) :role :alias :prop k, :form form :parent parent}]
                               (= `hf/link k) (rec [k `(~form ~'%)])
                               #_             (reference? form)
                               :else          [{:id (id-gen) :role :ref :prop k, :form form :parent parent}] ;; ref target will be resolved later.
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
               :symbol (if (meta-keyword? form)
                         (let [id    (id-gen)
                               sym   (merge {:id id :parent parent} (describe form))
                               metas (meta-points id-gen id form)]
                           (vec (cons sym metas)))
                         (update (rec parent `(~form ~'%)) 0 assoc :original-form form))
               :seq    (let [id    (id-gen)
                             call  (merge {:id id :parent parent} (describe form))
                             args  (args-points id-gen id form)
                             metas (meta-points id-gen id form)]
                         (vec (cons call (concat args metas))))
               :vector (vec (mapcat identity (map-indexed (fn [idx form]
                                                            (-> (rec parent form)
                                                                (assoc-in [0 :position] idx))) form)))
               :map    (vec (mapcat (fn [[k v]]
                                      (let [k-branch  (rec parent k)
                                            parent-id (:id (first k-branch))
                                            options   (first (filter (comp #{`hf/options} :prop) k-branch))]
                                        (cond-> (into k-branch (rec parent-id v))
                                          (some? options) (into (remove :prop (rec (:id options) v))))))
                                    form))))]
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
                          (= `hf/link (:prop point)) (update point :deps conj (:parent point))
                          :else                    point)
                        :else                        (update point :deps conj (:parent point)))))))))

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
                     (last)
                     (:id)))
              points)))

;; pass
(defn unique-name-points [points]
  (reduce (fn [r scope]
            (->> (group-by :name scope)
                 (mapcat (fn [[name points]]
                           (case (count (remove (comp #{:ref :arg} :role) points))
                             (0 1) points
                             (map-indexed (fn [idx point] (assoc point :name (symbol (str name "_" idx)), :occurrence idx)) points))))
                 (concat r)
                 ))
          () (vals (scopes points))))

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
    (map (fn rec [point] (assoc point :name (symbol (case (:role point)
                                                      :arg   (str (:name (rec (get index (:parent point))))
                                                                  "_"
                                                                  (name (:arg-name point)))
                                                      :input (str (:name (rec (get index (:parent point))))
                                                                  "_"
                                                                  "input")
                                                      (cond-> (str (munge (identifier point)))
                                                        (and (:prop point)
                                                             (#{`hf/options `hf/link} (:prop point))) (str "_" (munge (name (:prop point))))))))) points)))

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
                        parent-sym (if (nil? parent)
                                     `hf/entity
                                     (if (= :many (:card parent)) '% (list `unquote (:name parent))))]
                    `(hf/nav (:db hf/db) ~parent-sym ~(keyword (:form point))))]
      :call  [nom (let [[f & _args] (:form point)
                        args-name   (map symbol (map :name (spec/args f)))
                        defaultsf   (get (:props point) ::hf/defaults)
                        args        (->> (map index (:deps point))
                                         (filter #(= :arg (:role %)))
                                         (sort-by :position)
                                         (map (fn [point]
                                                (let [[_call parent] (lineage index point)
                                                      many?          (= :many (:card parent))]
                                                  (if (= '% (:form point))
                                                    (if many? '% `(unquote ~(:name parent)))
                                                    #_(if (some? parent)
                                                        `(unquote ~(:name parent)))
                                                    `(unquote ~(:name point)))))))]
                    `(var ~(cond
                             (some? defaultsf)          `(let [[~@args-name] (p/$ ~defaultsf [~@args])]
                                                           (p/$ ~f ~@args-name))
                             (= `hf/link (:prop point)) `(list '~f ~@args)
                             :else                      `(p/$ ~f ~@args))))]
      :arg   (when-not (= '% (:form point))
               [nom (let [form      (if-let [refs (some->> (map index (:deps point))
                                                           (remove #(:external (meta (:form %))))
                                                           (map (fn [point] [(:form point) (if (= :input (:role point))
                                                                                             `(unquote (m/watch ~(:name point)))
                                                                                             `(unquote ~(:name point)))]))
                                                           (seq)
                                                           (into {}))]
                                      (replace* refs (:form point))
                                      (if (meta-keyword? (:form point))
                                        (keyword (:form point))
                                        (:form point)))
                          defaultsf (get (:props point) ::hf/defaults)]
                      (list 'var
                            (cond
                              (some? defaultsf) `(p/$ ~defaultsf ~form)
                              :else             form)))])
      :input [nom `(atom nil)]
      :alias [nom (:name (get index (:parent point)))]
      :ref   nil)))

(defn roots [points]
  (let [present? (set (map :id points))]
    (remove #(present? (:parent %)) points)))

(defn symbolic [form]
  (condf form
    keyword?      form
    meta-keyword? (keyword form)
    symbol?       (list 'quote form)
    seq?          (list 'quote form)
    nil?          nil))

(defn symbolic-key [point] (symbolic (or (:prop point) (:original-form point) (:form point))))

(defn symbolic-prop [form]
  (condf form
    meta-keyword? (keyword form)
    symbol?       form
    seq?          form
    (symbolic form)))

(defn renderables [points]
  (remove (fn [point] (#{:alias :ref :arg :input} (:role point))) points))

(declare emit emit-render)

(def ^:dynamic *scopes*)
(def ^:dynamic *index*)
(def ^:dynamic *props?* false)

(defmacro render* [e a >v props]
  `(let [>v#    ~>v
         props# ~props]
     (binding [hf/context (cons [~e ~a #'(p/$ hf/data >v#) props#] hf/context)]
       (p/$ hf/render >v# props#))))

(defn var-point [form] (if *props?* form `(var ~form)))

(defn render-point
  ([>v props] (render-point nil nil >v props))
  ([e a >v props] (if *props?* >v `(render* ~(or e '(var nil)) ~a ~>v ~props))))

(defn columns [points] (mapv symbolic-key (sort-by :position (remove #(or (:prop %) (= :arg (:role %))) points)))) ;; FIXME remove predicate is flipped, columns can’t be props and should be either call or nav. 

(declare emit*)

(defn emit-1 [point]
  (binding [*props?* false #_ (or *props?* (some? (:prop point)))]
    (let [nom      (:name point)
          attr     (symbolic-key point)
          children (renderables (get-children *index* point))
          cont     (if (contains? *scopes* (:id point))
                     (partial emit* (:id point))
                     (fn [_ _]
                       (if-some [children (seq (remove :prop children))]
                         (into {} (map emit-1 children))
                         (if (= :many (:card point))
                           '%
                           nom))))]
      (if (and (:prop point) (not (#{`hf/options} (:prop point)))) #_*props?*
        [attr (cond
                (= :many (:card point)) `(p/for [~'% (unquote ~nom)]
                                           ~(cont nil nil))
                (seq children)          (cont nil nil)
                :else                   (if-let [prop (:prop point)]
                                          (cond (#{`hf/link} prop)          [(list 'quote (:form point)) nom]
                                                (#{`hf/as} prop)            (symbolic (:form point))
                                                (not (#{`hf/options} prop)) (symbolic-prop (:form point))
                                                :else                       `(unquote ~nom))
                                          `(unquote ~nom)))]
        (let [props  (some->> (get-children *index* point)
                              (filter :prop)
                              (map emit-1)
                              (remove (fn [[k _v]] (#{::hf/defaults} k)))
                              (map (fn [[k v]] [k (if false #_(#{::hf/options} k) `(var ~v) v)]))
                              (seq)
                              (into {}))
              parent (get *index* (:parent point) {:name (list 'var `hf/entity)})
              cols   (columns children)
              next   (render-point (if (= :many (:card parent)) '(var %) (:name parent))
                                   (symbolic (:form point))
                                   (cond
                                     (= :many (:card point))       `(var (p/for [~'% (unquote ~nom)]
                                                                           (var ~(cont '(var %) nil))))
                                     (seq (remove :prop children)) `(var ~(cont nil nil))
                                     :else                         nom)
                                   (cond-> props
                                     (seq cols) (assoc ::hf/columns cols)))]
          [attr (var-point next)]))))) 

(defn emit-render [points] (into {} (map emit-1 (remove :prop (roots points)))))

(defn toposort
  ([points]       (toposort (map-by :id points)))
  ([index points] (sort-by (fn [%] [(rank index %) (:id %)]) points)))

(defn emit-inputs [points]
  (when-let [inputs (not-empty (reduce (fn [r point]
                                         (let [[call parent & _] (lineage *index* point)
                                               input             (first (filter #(= :input (:role %)) (get-children *index* point)))]
                                           (assoc-in r [(symbolic (:form parent)) (symbolic (:form call)) (:arg-name point)]
                                                     [(:name point) (when (some? input)
                                                                      (:name input))])))
                                       {} (filter (fn [%] (and (= :arg (:role %))
                                                               (not= `hf/link (:prop (get *index* (:parent %)))))) points)))]
    {::hf/inputs inputs}))

(defn emit*
  ([point-id] (emit* point-id (list 'var `hf/entity) `hf/attribute))
  ([point-id e a]
   (let [points (toposort *index* (get *scopes* point-id))]
     `(let [~@(mapcat #(emit-point *index* %) (remove (fn [point] (#{`hf/defaults `hf/render} ;; FIXME kw vs sym vs meta-kw
                                                                   (:prop point)))
                                                      points))]
        ~(if (and (nil? point-id) (= 1 (count (remove :prop points)))) ;; no traversal, root expr, like: (hfql :db/id)
           (second (val (first (emit-render (renderables points)))))
           (render-point e a (var-point (emit-render (renderables points)))
                          (cond-> (emit-inputs points)
                            (nil? point-id) (assoc ::hf/columns (columns (roots points))))))))))

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
       name-points
       unique-name-points
       ))

(defn hfql [&env form]
  (->> form
       (env/resolve-syms (env/make-env &env))
       parse
       analyze
       emit
       ))

;; (comment

  

;;   (defn breadcrumbs [context]
;;     (let [human-friendly-points (filter (fn [[e a _v]] (or e a)) context)
;;           index                 (group-by first human-friendly-points)]
;;       (reduce (fn [r e]
;;                 (cons e (into r (filter identity (map second (get index e))))))
;;               () (dedupe (map first human-friendly-points)))))

;;   (defn format-breadcrumbs [path]
;;     (str/join " > " (cons "/" (rest path))))

;;   (p/defn 🦆 [>v props]
;;     (prn (str "Breadcrumbs: " (format-breadcrumbs (breadcrumbs (p/for [[>e a >v] hf/context] [~>e a >v])))))
;;     (p/$ render >v (dissoc props ::hf/render)))

;;   (tests
;;    (p/run (! (hfql [{(user.gender-shirt-size/submissions .) [{:dustingetz/gender [(props :db/ident {::hf/render 🦆})]}]}])
;;              ))
;;    % := _)


;;   (p/defn render-options [>v props]
;;     (into [:select {:value (p/$ render >v (dissoc props ::hf/render))}]
;;           (p/for [opt ~(::hf/options props)]
;;             [:option opt])))

;;   ;; DONE
;;   (tests
;;    (p/run (! (hfql [{^{::hf/defaults (p/fn [[needle]] [(or needle "alice")])}
;;                      (user.gender-shirt-size/submissions .)
;;                      [:dustingetz/email
;;                       ;; (user.gender-shirt-size/shirt-sizes . .)
;;                       {(props (:dustingetz/shirt-size %) {::hf/options (user.gender-shirt-size/shirt-sizes gender .)
;;                                                           ;; ::hf/option-label :db/ident
;;                                                           ::hf/render render-options
;;                                                           })
;;                        [:db/id]}
;;                       {(props :dustingetz/gender {::hf/options (user.gender-shirt-size/genders)
;;                                                   ::hf/render render-options
;;                                                   })
;;                        [(props :db/ident {::hf/as gender})]}]}
;;                     #_{(user.gender-shirt-size/genders)
;;                        [:db/ident #_(props :db/ident {::hf/as gender})]}]) 
;;              )) 
;;    % := _)



;;   (p/defn suber-name [e] (first (str/split ~(hf/nav e :dustingetz/email) #"@")))
;;   (s/fdef suber-name :args (s/cat :e any?) :ret any?)

;; (p/defn blank [])
;; (s/fdef blank :args (s/cat) :ret any?)

;; DONE
;; (tests
;; (p/run (! (hfql [{(user.gender-shirt-size/submissions "alice") [(suber-name %)
;;                                                                 (props :db/id {::hf/link (blank)})
;;                                                                 suber-name]}]) 
;;           ))
;; % := _)

;;   (tests
;;    (p/run (! (hfql [{(user.gender-shirt-size/submissions .)
;;                      [(props :db/id {::hf/link user.gender-shirt-size/sub-profile})
;;                       (props :dustingetz/email {::hf/link (user.gender-shirt-size/sub-profile db/id)})]}]) 
;;              ))
;;    % := _) 



;;   (comment
;;     (tests
;;      ;; Call inner fn with incorrect arity, %2 arg binding (b) leaks from parent fn
;;      (p/run (! (p/$ (p/fn [a b] (p/$ (p/fn [c d] [c d]) 3))
;;                     1 2)))
;;      % := _ ;; FAIL Should throw, instead return [3 2]
;;      ))

;;   ;; (rcf/enable! )

;;   ;; (alter-var-root #'clojure.pprint/*print-miser-width* (constantly 72))
;;   ;; (alter-var-root #'clojure.pprint/*print-right-margin* (constantly 80))
;;   ;; (alter-var-root #'clojure.pprint/*print-pprint-dispatch* (constantly clojure.pprint/code-dispatch))



;;   )


