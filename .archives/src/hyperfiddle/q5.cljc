(ns ^{:clj-kondo/config '{:lint-as {hfdl.lang/def  clojure.core/def
                                    hfdl.lang/defn clojure.core/defn}}}
    hyperfiddle.q5
  (:refer-clojure :exclude [read compile resolve])
  (:require [cljs.analyzer.api :as ana-api]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests ! %]]
            [hyperfiddle.spec :as spec]
            [missionary.core :as m]
            [user.gender-shirt-size :refer [submissions submission shirt-sizes genders emails]])
  #?(:cljs (:require-macros [hyperfiddle.q5 :refer [hfql apply-by-name]])))

(do
  (s/def ::literal    any?)
  (s/def ::keyword    qualified-keyword?)
  (s/def ::symbol     symbol?)
  (s/def ::prop       (s/cat ::prop keyword?, ::form any?))
  (s/def ::props      (s/and seq? (s/cat ::form (s/alt ::symbol symbol?, ::edge ::edge ), ::_dot #{'.} ::props (s/* ::prop))))
  (s/def ::arg        (s/or ::traversal ::traversal, ::props ::props, ::symbol ::symbol ::literal any?))
  (s/def ::call       (s/and seq?
                             #(= ::s/invalid (s/conform ::props %))
                             (s/cat ::f symbol? ::args (s/* ::arg))))
  (s/def ::edge       (s/or ::keyword qualified-keyword?, ::props ::props, ::call ::call, ::traversal ::traversal))
  (s/def ::edges      (s/coll-of ::edge :kind vector? :min-count 1))
  (s/def ::traversal  (s/and (s/map-of ::edge ::edges :conform-keys true) #(= 1 (count %))))
  (s/def ::hfql       (s/or ::traversal ::traversal, ::call ::call, ::props ::props, ::edge ::edge, ::edges ::edges)))

(tests
  (s/conform ::prop '(::a 1)) := {::prop ::a, ::form 1})

(tests
  (s/conform ::props '(a . ::a 1)) := '{::form  [::symbol a],
                                        ::_dot  .,
                                        ::props [{::prop ::a, ::form 1}]})

(tests
  (s/conform ::call '(f x)) := {::f 'f, ::args [[::symbol 'x]]})

(tests
  (s/conform ::edge  :a)          := ::s/invalid
  (s/conform ::edge ::a)          := [::keyword ::a]
  (s/conform ::edge '(f x))       := [::call {::f 'f, ::args [[::symbol 'x]]}]
  (s/conform ::edge '(a . ::a 1)) := [::props {::form [::symbol 'a], ::_dot '., ::props [{::prop ::a, ::form 1}]}])

(tests
  (s/conform ::traversal {::a []})               := ::s/invalid
  (s/conform ::traversal {::a [::b]})            := {[::keyword ::a] [[::keyword ::b]]}
  (s/conform ::traversal '{(a . ::a b) [::c]})   := '{[::props {::form [::symbol a], ::_dot ., ::props [{::prop ::a, ::form b}]}]
                                                   [[::keyword ::c]]}
  (s/conform ::traversal '{a [::b]})             := ::s/invalid
  (s/conform ::traversal {::a [::b], ::c [::d]}) := ::s/invalid)

(defn read [form]
  (let [conformed (s/conform ::hfql form)]
    (if (= ::s/invalid conformed)
      (throw (ex-info "Invalid syntax" {::type ::invalid-syntax
                                        ::data (s/explain-data ::hfql conformed)}))
      conformed)))

(tests
  (read '{::a [::b]})             := [::traversal _]
  (read '::a)                     := [::edge _]
  (read [::a])                    := [::edges _]
  (read '(f x))                   := [::call _]
  (read '(a . ::a 1))             := [::props _]
  (read '(f (a . ::a 1)))         := [::call {::f 'f, ::args [[::props _]]}]
  (read '(f {(a . ::a 1) [::b]})) := [::call {::f 'f, ::args [[::traversal {[::props _] [[:hyperfiddle.q5/keyword :hyperfiddle.q5/b]]}]]}]
  )

(defn parse [[type form]]
  (-> (case type
        ::traversal (let [edge  (parse (key (first form)))
                          edges (parse [::edges (val (first form))])]
                      {::type  type
                       ::edge  edge
                       ::edges (parse [::edges (val (first form))])
                       ::form  form})
        ::edges     {::type  type
                     ::edges (mapv #(parse [::edge %]) form)}
        ::edge      (parse form)
        ::keyword   {::type ::keyword-edge
                     ::form form}
        ::props     {::type  type
                     ::form  (parse (::form form))
                     ::props (mapv #(parse [::prop %])(::props form))}
        ::prop      {::type type
                     ::prop (::prop form)
                     ::form (::form form)}
        ::call      {::type type
                     ::f    (::f form)
                     ::args (mapv parse (::args form))}
        ::symbol    {::type type
                     ::form  form}
        ::literal   {::type ::literal
                     ::form  form})
      (assoc ::source form)))

(tests
  (::type (parse (read '(a . ::a 1)))) := ::props ; not a fn call
  ;; non-namespaced keywords are exceptionaly allowed in props.
  (::type (parse (read '(a . :a 1)))) := ::props)

(defn unparse
  ([ast] (unparse ast false))
  ([ast drop-args-props?]
   (case (::type ast)
     ::traversal            {(unparse (::edge ast)) (unparse (::edges ast))}
     (::keyword-edge ::nav) (s/unform ::keyword (::form ast))
     ::call                 (if drop-args-props?
                              (unparse (update-in ast [::source ::args] #(mapv (fn rec [arg]
                                                                                 (case (first arg)
                                                                                   ::props (::form (second arg))
                                                                                   ::traversal (rec (ffirst (second arg)))
                                                                                   arg)) %)))
                              (s/unform (::type ast) (::source ast)))
     (s/unform (::type ast) (::source ast)))))

(let [roundtrip (comp unparse parse read)]
  (tests
    (roundtrip ::a)                     := ::a
    (roundtrip [::a])                   := [::a]
    (roundtrip {::a [::b]})             := {::a [::b]}
    (roundtrip '(f x))                  := '(f x)
    (roundtrip '(a . ::a 1))            := '(a . ::a 1)
    (roundtrip '(f (a . ::a 1))         := '(f (a . ::a 1)))
    (roundtrip '(f {(a . ::a 1) [::b]}) := '(f {(a . ::a 1) [::b]}))))


(defn identifier [ast]
  ;; #dbg ^{:break/when (= nil (::type ast))}
  (case (::type ast)
    ::keyword-edge [::keyword-edge (::form ast)]
    ::edges        [::edges (mapv identifier (::edges ast))]
    ::traversal    [::traversal (identifier (::edge ast)) (identifier (::edges ast))]
    ::call         [::call (unparse ast true)]
    ::props        (identifier (::form ast))
    ::symbol       [::symbol (unparse ast)]
    ::nav          [::nav (::form ast)]
    ))

(tests
  (identifier (parse (read ':db/id)))                  := [::keyword-edge :db/id]
  (identifier (parse (read '[:db/id])))                := [::edges [[::keyword-edge :db/id]]]
  (identifier (parse (read {::a [::b]})))              := [::traversal [::keyword-edge ::a] [::edges [[::keyword-edge ::b]]]]
  (identifier (parse (read '(f x))))                   := [::call '(f x)]
  (identifier (parse (read '(a . ::a 1))))             := [::symbol 'a]
  (identifier (parse (read '(f (a . ::a 1)))))         := [::call '(f a)]
  (identifier (parse (read '(f {(a . ::a 1) [::b]})))) := [::call '(f a)]
  )


(defn walk-nodes
  ([ast f]
   ;; inefficient but good enough
   (walk/prewalk (fn [x] (if (and (map? x) (::type x)) (f x) x)) ast))
  ([ast type-pred f]
   (walk-nodes ast (fn [x] (if (type-pred (::type x)) (f x) x)))))

;;;;;;;;;;;;
;; PASSES ;;
;;;;;;;;;;;;

(defn resolve-global [env x]
  (when (symbol? x)
    (if (:js-globals env)
      (:name (ana-api/resolve env x))
      (when-let [?var (get (::globals env) x)]
        (if (var? ?var)
          (symbol ?var)
          x)))))

(defn qualify-syms-pass "resolve all symbols to their fully qualified names" [env ast]
  (let [env (atom env)]
    (walk-nodes ast #{::prop} (fn [{:keys [::prop ::form] :as property}]
                                (when (= ::hf/as prop)
                                  (swap! env assoc [::alias form] form))
                                property))
    (let [env @env]
      [env (walk/prewalk (fn [x]
                           (if (and (simple-symbol? x) (not (contains? env [::alias x])))
                             (or (resolve-global env x) x)
                             x)) ast)])))

(defn props-on-form-pass "Flatten ::props, moving the props info to the LHS form" [ast]
  (walk-nodes ast #{::props} (fn [{:keys [::form ::props] :as _ast}] (assoc form ::props props))))

(defn edges->nav-pass "transform all kw edges into a nav" [ast]
  (walk-nodes ast #{::keyword-edge} (fn [{:keys [::form ::props]}] {::type  ::nav
                                                                    ::form  form
                                                                    ::props props})))

(defn symbolic-traversal-pass
  "Craft a symbolic representation of a traversal that can be used as a map key.
  e.g.: {::a […]} := ::a
        {(f) […]} := '(f)"
  [ast]
  (-> (walk-nodes ast #{::call} (fn [ast] (assoc ast ::form (list 'quote (unparse ast)))))
      (walk-nodes #{::traversal} (fn [{:keys [::edge] :as ast}]
                                   (assoc ast ::form (unparse edge))))))

(defn named-traversal-pass " Ensures only ::edges is responsible of collecting edges in a map. Wrap a
  top-level ::traversal T into ::edges [T]"
  [ast]
  (if (= ::traversal (::type ast))
    {::type  ::edges
     ::edges [ast]}
    ast))

(defn replace-aliases [env form]
  (map (fn [x] (get env [::alias x] x)) form))

(defn resolve-local [env x] (get env x))

(defn unbounded-sym? [env x]
  (and (simple-symbol? x)
       (or (contains? env [::alias x]) ;; aliases shadows local and global env
           (and
            (not (resolve-local env x))
            (not (resolve-global env x))))
       x))

;; FIXME uniformize `free-variable?` and `unbounded-sym?`
(defn free-variable? [env ast]
  (and (= ::symbol (::type ast))
       (unbounded-sym? env (::form ast))))

(defn rewrite-remaining-free-inputs [env form]
  (->> form
       (map (fn [x] (if (unbounded-sym? env x)
                      `(get hf/args '~x)
                      x)))))

;; When true, bypasses custom renderers at runtime.
;; Useful to get the  value of an aliased form as intended by the pull expression.
(p/def bypass-renderers false)

(declare form->ast emit)

(defn call-hf-options-pass "Call ::hf/options with p/$"
  [env ast]
  (let [aliases (atom #{})]
    (walk-nodes ast #{::prop} (fn [{:keys [::prop ::form ::edges] :as ast}]
                                (if (= ::hf/options prop)
                                  (let [call (rewrite-remaining-free-inputs env (replace-aliases env form))]
                                    (assoc ast ::compiled-form `(var (binding [bypass-renderers true]
                                                                       ~(if (some? edges)
                                                                          (let [ast (-> (form->ast env `{~form ~(unparse edges)})
                                                                                        (walk-nodes #{::vars} ::body)) ;; drop vars, already computed in parent ast
                                                                                ]
                                                                            `(unquote (val (first (unquote ~(emit env ast))))))
                                                                          (cons `p/$ call))))))
                                  ast)))))

(defn ->free-var [path ast] {::type ::var, ::sym (::form ast), ::alias (symbol (str (::form ast) "_form")) ::path path, ::props (::props ast)})

(defn- dig [path point] (if (::stubbed (meta path))
                          path
                          (conj path (hash point))))

(defn- stub [path] (clojure.core/with-meta path {::stubbed true}))

(defn- forks-here? [path1 path2]
  (and (= (first path1) (first path2))
       (not= (second path1) (second path2))))

(def ^:dynamic *context*)

(defn free-variables-here
  ([env ast] (let [vars           (free-variables-here [] env ast)        ; find all free variables from this point
                   declared-here  (filter #(= 1 (count (::path %))) vars) ; keep the ones immediately at the root (here)
                   vars           (remove (set declared-here) vars)       ; focus on deeper ones
                   diverging-here (for [v     vars ; only keep deeper vars that start to diverge here
                                        w     vars
                                        :when (and (not= v w)
                                                   (= (::sym v) (::sym w))
                                                   (forks-here? (::path v) (::path w)))]
                                    v)]
               (concat declared-here diverging-here)))
  ([path env ast]
   (-> (case (::type ast)
         ;; Walk the AST looking for symbols, intersecting parallel branches.
         ;; Could be tightened with a better AST walking abstraction.
         ::edges        (apply set/union (map (partial free-variables-here (dig path ast) env) (::edges ast)))
         ::call         (binding [*context* ::call]
                          (->> (::args ast)
                               (map (partial free-variables-here (dig path ast) env))
                               (apply set/union)
                               (map #(assoc % ::origin ::call ::path path, ::f (::f ast) ::alias (::sym %)))))
         ::prop         (->> (::form ast)
                             (tree-seq coll? identity)
                             (remove #{'var})
                             (filter (partial unbounded-sym? env)) ; FIXME should be `free-variable?`
                             (map #(assoc {} ::form %))            ; FIXME should be done in the parse/transform phase
                             (map (partial ->free-var path))
                             (map (fn [var] (case (::prop ast)
                                              ::hf/as      (assoc var ::origin ::alias)
                                              ::hf/options (assoc var ::origin ::reference)
                                              var))))
         ::traversal    (concat (cond->> (free-variables-here (dig path ast) env (::edge ast))
                                  (= ::call *context*) (map #(assoc % ::edges (::edges ast))))
                                (free-variables-here (dig path ast) env (::edges ast)))
         ::edge         (free-variables-here (dig path ast) env ast)
         ::keyword-edge nil
         ::nav          (mapcat (partial free-variables-here (dig path ast) env) (::props ast))
         ::props        (concat (free-variables-here (dig path ast) env (::edge ast))
                                (mapcat (partial free-variables-here (dig path ast) env) (::props ast)))
         ::symbol       (when (free-variable? env ast)
                          #{(->free-var path ast)})
         ::literal      nil)
       (set))))

(defn difference-by [f xs ys] (set (remove (set (map f ys)) xs)))

(tests
  (difference-by identity #{1 2} #{2}) := #{1})

(defn pick-vars-introducing-bindings [vars]
  (let [[aliases call _refs] (mapv set ((juxt ::alias ::call) (group-by ::origin vars)))]
    (->> (concat aliases (difference-by ::sym call aliases))
         (sort #(compare (.indexOf vars %1) (.indexOf vars %2))))))

(defn toposort-free-variables-pass
  "Symbols that can not be resolved from the parent env are considered free variables.
  The first occurrence introduces a lexical binding at the closest common
  ancestor of all instances of the free variable. Further references refers to the lexical binding."
  [env ast]
  (let [!visited (volatile! #{})]
    (walk-nodes ast #{::traversal ::edges}
                (fn [ast]
                  (if (contains? @!visited (hash ast))
                    ast
                    (if-some [vars (seq (free-variables-here env ast))]
                      (if-some [vars (seq (pick-vars-introducing-bindings vars))]
                        (do (vswap! !visited conj (hash ast))
                            {::type ::vars
                             ::vars vars
                             ::body ast})
                        ast)
                      ast))))))

(defn find-by-hash [positionf x hashes]
  (->> (tree-seq coll? identity x)
       (map (juxt identity hash))
       (filter (comp (set hashes) second))
       (remove (comp #{::edges} ::type first))
       (positionf)
       (first)))

(defn path-of [node ast]
  (cond
    (nil? ast)   nil                 ; no path to node
    (= node ast) (list (::form ast)) ; node path found
    :else        (case (::type ast)
                   ::traversal (cons (::form (::edge ast)) (path-of node (::edges ast)))
                   ::edges     (first (filter identity (map (partial path-of node) (::edges ast)))))))

(defn lazy-getter [sym path]
  (apply list '-> sym `(unquote) `(unquote) (interpose `(unquote) (map #(list 'get %) path))))

(declare form->ast)

(defn move-ast-to-free-var-binding-pass [env ast]
  (let [!env (volatile! env)
        ast  (walk-nodes ast #{::vars} (fn [{:keys [::vars ::body] :as ast}]
                                         (let [vars (mapcat (fn [var]
                                                              (let [form  (find-by-hash first body (::path var))
                                                                    point (find-by-hash last body (::path var))]
                                                                (case (::origin var)
                                                                  ::alias       (do (vswap! !env assoc [::alias (identifier form)]        (::alias var))
                                                                              (vswap! !env assoc [::alias (::sym var)] (list `unquote (::sym var)))
                                                                              [(assoc var ::body form)
                                                                               (assoc var ::alias (::sym var), ::body (-> (form->ast env (lazy-getter (::alias var) (rest (path-of point form))))
                                                                                                                          (update ::form second) ; drop quote on the fn form
                                                                                                                          ))])
                                                                  ::call        (do (vswap! !env assoc [::alias (::sym var)] (list `unquote (::sym var)))
                                                                             [(assoc var ::sym (::alias var) ::body form)])
                                                                  #_::reference ; should never happen here.
                                                                  )))
                                                            vars)]
                                           (assoc ast ::vars vars, ::form (::form body)))))]
    [@!env ast]))

(defn prop-index [prop-kw props]
  (->> (map-indexed vector props)
       (filter (fn [[idx prop]]
                 (= prop-kw (::prop prop))))
       (ffirst)))

(defn pullexpr-on-options-pass
  "hf/options should be pulled with the same pullexpr as the traversal they are part of."
  [ast]
  (walk-nodes ast #{::traversal} (fn [{:keys [::edge ::edges] :as ast}]
                                   (if (seq (::props edge))
                                     (if-let [idx (prop-index ::hf/options (::props edge))]
                                       (update-in ast [::edge ::props idx] assoc ::edges edges)
                                       ast)
                                     ast))))

(defn transform [env ast]
  (let [[env ast] (qualify-syms-pass env ast)
        ast       (->> ast
                       props-on-form-pass
                       edges->nav-pass
                       symbolic-traversal-pass
                       named-traversal-pass
                       (toposort-free-variables-pass env))
        [env ast] (move-ast-to-free-var-binding-pass env ast)
        ast       (pullexpr-on-options-pass ast)
        ast       (call-hf-options-pass env ast)
        ]
    [env ast]))

(defn clean-props [props] (dissoc props ::hf/render ::hf/as))

(defn forms [ast]
  (case (::type ast)
    ::edges (::edges ast)
    ::vars  (forms (::body ast))
    ast))

(defn inputs-here [vars]
  (mapv ::sym (filter (comp #{::call} ::origin) vars)))

(defn unparse-call
  "Like `unparse`, but drop props on call args"
  [{:keys [::type ::f ::args]}]
  (assert (= ::call type))
  (cons f (map (fn [arg] (unparse (case (::type arg)
                                    ::traversal (::edge arg)
                                    ::props     (::form arg)
                                    arg))) args)))

(defn unparse-edge
  "Like `unparse` but drop props on call args"
  [ast]
  (case (::type ast)
    ::vars      (unparse-edge (::body ast))
    ::traversal (unparse-edge (::edge ast))
    ::call      (unparse-call ast)
    (unparse ast)))

(tests
  (unparse-call (parse (read '(f (a . ::a 1))))) := '(f a))

(defn escape [form]
  (if (or (symbol? form) (seq? form))
    (list 'quote form)
    form))

;; (defn get-arg [var-ast]
;;   (assert (= ::var (::type var-ast)))
;;   (assert (= ::call (::origin var-ast)))
;;   (let [sym (::sym var-ast)]
;;     (->> var-ast ::body ::edge ::args
;;          (filter #(= sym (::form %)))
;;          (first))))

(defn map-entry [k v] (first {k v}))

(defn get-form [ast]
  (or (get ast ::compiled-form)
      (get ast ::form)))

;; (defn foo-meta "Flipped `clojure.core/with-meta` for better readability."
;;   [m o]
;;   (clojure.core/with-meta o m))

(defn emit [env ast]
  ;; NOTE Deduplication is intentional to avoid premature optimization.
  (let [renderer (fn [props] (if-let [render (::hf/render props)]
                               (if (= `hf/render render)
                                 `hf/render
                                 `(if bypass-renderers hf/render ~render))
                               `hf/render))]
    (case (::type ast)
      ::nav   (let [props (zipmap (map ::prop (::props ast)) (map get-form (::props ast)))]
                `(var (binding [hf/value (var (unquote (hf/nav ~'entity ~(get-form ast))))
                                hf/props ~(clean-props props)]
                        (unquote ~(renderer props)))))
      ::edges (let [inputs    (zipmap (map (fn [ast] (list 'quote (unparse-edge ast))) (::edges ast))
                                      (->> (::edges ast)
                                           (map ::vars)
                                           (map (fn [vars] (mapv (fn [var] (map-entry (list 'quote (::sym var)) (emit env var)))
                                                                 vars)))))
                    edges-map (zipmap (map (fn [ast] (escape (unparse-edge ast))) (::edges ast)) (map (partial emit env) (::edges ast)))]
                (if (seq inputs)
                  `(var (with-meta ~edges-map  {::inputs ~inputs}))
                  `(var ~edges-map)))
      ::var   (let [ast      (call-hf-options-pass (dissoc env [::alias (::sym ast)]) ast)
                    props    (::props ast)
                    props'   (zipmap (map ::prop props) (map ::form props))
                    renderer (::hf/render props')]
                (if (some? renderer)
                  (if (and (::edges ast) (::hf/options props'))
                    (let [traversal {::type ::traversal, ::edges (::edges ast), ::edge (parse (read (::hf/options props')))}]
                      `(var (binding [hf/props ~(-> (dissoc props' ::hf/render)
                                                    (assoc ::hf/options (emit env traversal)))]
                              (unquote ~(::hf/render props')))))
                    `(var (binding [hf/props ~(-> (zipmap (map ::prop props) (map get-form props))
                                                  (dissoc ::hf/render))]
                            (unquote ~(::hf/render props')))))
                  `#'nil))

      ::traversal (let [edge  (::edge ast)
                        props (zipmap (map ::prop (::props edge)) (map get-form (::props edge)))]
                    (case (::type edge)
                      ::call (case (spec/cardinality (::f edge))
                               nil         (throw (ex-info (str "Can’t infer cardinality. Please provide a spec for `" (pr-str (::f edge)) "`") {}))
                               ::spec/many `(var (binding [hf/columns '~(mapv get-form (forms (::edges ast)))
                                                           hf/value   (var (p/for [~'entity ~(cons `p/$ (rewrite-remaining-free-inputs env (replace-aliases env (unparse-call edge))))]
                                                                             (var (binding [hf/value ~(emit env (::edges ast))
                                                                                            hf/props {}]
                                                                                    (unquote hf/render)))))
                                                           hf/props   ~(clean-props props)]
                                                   (unquote ~(renderer props))))
                               ::spec/one  `(let [~'entity ~(cons `p/$ (replace-aliases env (unparse-call edge)))]
                                              (var (binding [hf/value ~(emit env (::edges ast))
                                                             hf/props ~(clean-props props)]
                                                     (unquote ~(renderer props))))))
                      ::nav  (if-let [alias (get env [::alias (identifier ast)])]
                               (list `unquote alias)
                               `(let [~'entity (unquote (hf/nav ~'entity ~(get-form edge)))]
                                  (var (binding [hf/columns '~(mapv get-form (forms (::edges ast)))
                                                 hf/value   ~(emit env (::edges ast))
                                                 hf/props   ~(clean-props props)]
                                         (unquote ~(renderer props))))))))
      ::vars      (let [inputs (inputs-here (::vars ast))
                        env'   (reduce (fn [r var] (if (::body var)
                                                     (dissoc r [::alias (identifier (::body var))])
                                                     r)) env (::vars ast))
                        env''  (reduce (fn [r var] (if ((set inputs) (::sym var))
                                                     (dissoc r [::alias (::sym var)])
                                                     r))
                                       env (::vars ast))]
                    `(let [~@(mapcat identity (map (fn [{:keys [::alias ::origin ::body] :as _var}]
                                                     (when (= ::alias origin)
                                                       [alias (case origin
                                                                #_#_::call `(var (p/$ hf/render-input '~(::f var) '~(::sym var)))
                                                                ::alias    (list `var (emit env' body)))]))
                                                   (::vars ast)))]
                       ~(emit env'' (::body ast))))
      ::call      (get-form ast))))

(defn local-binding? [v] (instance? clojure.lang.Compiler$LocalBinding v))

(defn env [&env]
  (let [env (into {} (reduce-kv (fn [r k v] (if (local-binding? v)
                                              (assoc r (list 'quote (.-sym v)) (.-sym v))
                                              (assoc r k v))) {} &env))]
    (assoc env ::globals (ns-map *ns*))))

(defn form->ast [env expr] (second (transform env (parse (read expr)))))

(defn compile [env expr] (->> expr read parse (transform env) (apply emit)))

(defmacro hfql [expr]
  `(binding [hf/value ~(compile (env &env) expr)]
     (unquote hf/render)))

;; TODO not sure if this is useful
(defmacro apply-by-name [f args-map]
  `(binding [hf/args ~args-map]
     (unquote ~f)))

(tests
  (compile (env {}) :db/id) := `(var (binding [hyperfiddle.api/value (var (unquote (hyperfiddle.api/nav ~'entity :db/id)))
                                               hyperfiddle.api/props {}]
                                       (unquote hyperfiddle.api/render))) )

(tests
  (p/run (! (let [entity 9] (hfql :db/id) )))
  % := 9)

(tests
  (p/run (! (let [entity 9] (hfql [:db/id]) )))
  % := {:db/id 9})

(p/defn id-as-string-renderer [] (str ~hf/value))

(tests
  (p/run (! (let [entity 9] (hfql (:db/id . ::hf/render id-as-string-renderer)) )))
  % := "9")

(tests
  (p/run (! (let [entity 9] (hfql [(:db/id . ::hf/render id-as-string-renderer)]) )))
  % := {:db/id "9"})

(comment

  (hyperfiddle.rcf/set-timeout! 1000)

  (defn foo [] (m/ap (m/? (m/via m/blk :foo))))

  (tests
    (p/run (! ~~#'(let [a ~(foo)]
                    #'a)))
    % := :foo ;; timeout
    )
  )


(tests
  (p/run (! (hfql {(submission "") [:db/id]}) ))
  % := {'(user.gender-shirt-size/submission "") {:db/id 9}})

(tests
  (p/run (! (hfql {(submission "") [{:dustingetz/shirt-size [:db/ident]}]}) ))
  % := {'(user.gender-shirt-size/submission "") #:dustingetz{:shirt-size #:db{:ident :dustingetz/womens-large}}})

(tests
  (p/run (! (hfql {(submissions "") [:db/id]}) ))
  % := {'(user.gender-shirt-size/submissions "") [{:db/id 9} {:db/id 10} {:db/id 11}]})

(tests
  (p/run (! (hfql {(submissions "") [(:db/id . ::hf/render id-as-string-renderer)]}) ))
  % := {'(user.gender-shirt-size/submissions "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})

(defn fail []
  (throw (ex-info "I fail" {})))

(p/def throwing-renderer #'(fail))

(p/def ignoring-renderer #'"ignored")

(tests
  (p/run (! (hfql {(submission "") [{(:dustingetz/gender . ::hf/render ignoring-renderer) [(:db/ident . ::hf/render throwing-renderer)]}]}) ))
  % := {'(user.gender-shirt-size/submission "") #:dustingetz{:gender "ignored"}} ; note it didn’t throw
  )

(p/def select-option-renderer
  #'(into [:select]
          (p/for [e ~(::hf/options hf/props)]
            [:option ~(hf/nav e :db/ident)])))

(tests
  (p/run (! (let [entity 9]
              (hfql [(:dustingetz/shirt-size . ::hf/render select-option-renderer
                                             ::hf/options (shirt-sizes :dustingetz/female "")
                                             )]) )))
  %
  := #:dustingetz{:shirt-size [:select
                               [:option :dustingetz/womens-small]
                               [:option :dustingetz/womens-medium]
                               [:option :dustingetz/womens-large]]})

;;;;;;;;;
;; ENV ;;
;;;;;;;;;

(tests
  (compile (env {}) '{(submissions needle) [:db/id]})
  := _)

(tests
  (compile (env {'needle '_}) '{(submissions needle) [:db/id]})
  := _)


(tests
  (p/run (let [entity 9]
           (! (hfql [(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender ""))
                     {:dustingetz/gender [(:db/ident . ::hf/as gender)]}]) )))
  % := #:dustingetz{:gender     #:db{:ident :dustingetz/female},
                    :shirt-size :dustingetz/womens-large})

(p/defn render-typeahead []
    [:select {:value ~hf/value}
     (p/for [e (apply-by-name (::hf/options hf/props) {'needle ""})]
       [:option ~(hf/nav e :db/ident)])])

(tests
  (p/run (let [needle "alice"]
           (! (hfql [{(submissions (needle . ::hf/render foo)) [:db/id]}]) )))
  % := '{(user.gender-shirt-size/submissions needle) [#:db{:id 9}]})

(p/defn submissions-query [needle]
  (hfql [{(submissions (needle . ::hf/render foo)) [:db/id]}]))

(tests
  (p/run (! (p/$ submissions-query "alice")))
  % := '{(user.gender-shirt-size/submissions needle) [#:db{:id 9}]})

(tests
  (p/run (! (hfql [{(submissions needle)
                    [:db/id
                     :dustingetz/email
                     (:dustingetz/shirt-size . ::hf/render render-typeahead
                                             ::hf/options (shirt-sizes gender ""))
                     {(:dustingetz/gender . ::a 1) [(:db/ident . ::hf/as gender)]}]}]) ))
  % := '{(user.gender-shirt-size/submissions needle)
         [{:dustingetz/gender #:db{:ident :dustingetz/female},
           :dustingetz/email  "alice@example.com",
           :dustingetz/shirt-size
           [:select
            {:value :dustingetz/womens-large}
            [[:option :dustingetz/womens-small]
             [:option :dustingetz/womens-medium]
             [:option :dustingetz/womens-large]]],
           :db/id             9}
          {:dustingetz/gender #:db{:ident :dustingetz/male},
           :dustingetz/email  "bob@example.com",
           :dustingetz/shirt-size
           [:select
            {:value :dustingetz/mens-large}
            [[:option :dustingetz/mens-small]
             [:option :dustingetz/mens-medium]
             [:option :dustingetz/mens-large]]],
           :db/id             10}
          {:dustingetz/gender #:db{:ident :dustingetz/male},
           :dustingetz/email  "charlie@example.com",
           :dustingetz/shirt-size
           [:select
            {:value :dustingetz/mens-medium}
            [[:option :dustingetz/mens-small]
             [:option :dustingetz/mens-medium]
             [:option :dustingetz/mens-large]]],
           :db/id             11}]})

;;;;;;;;;;;;;;;;;;;
;; VIEW-DEFAULTS ;;
;;;;;;;;;;;;;;;;;;;

;; Props on args

(tests
  (parse (read '(a . :b b)) )
  :=
  '{::type   ::props,
    ::form   {::type   ::symbol,
              ::form    a,
              ::source a},
    ::props  [{::type ::prop,
               ::prop :b,
               ::form b,
               ::source
               {::prop :b, ::form b}}],
    ::source {::form [::symbol a],
              ::_dot .,
              ::props
              [{::prop :b, ::form b}]}})

(tests
  (parse (read '(f (a . :b b))))
  :=
  '{::type   ::call
    ::f      f
    ::args   [{::type   ::props,
               ::form
               {::type   ::symbol,
                ::form    a
                ::source a}
               ::props  [{::type   ::prop
                          ::prop   :b
                          ::form   b
                          ::source _}]
               ::source _}]
    ::source _})


(tests
  (parse (read '(f {(a . :b b) [::a]})))
  := '{::type   :hyperfiddle.q5/call,
       ::f      f,
       ::args
       [{::type   :hyperfiddle.q5/traversal,
         ::edge   _
         ::edges  _
         ::form   _
         ::source _}],
       ::source _})

(p/def needle-options #'[9]) ;; Alice

(s/fdef needle-options :ret (s/coll-of any?))

(p/def needle-renderer #'(let [options (binding [hf/render hf/sequenceM] ~(::hf/options hf/props))]
                           (:dustingetz/email (first options))))

(p/def use-input-renderer
  #'(binding [hf/render hf/sequenceM] ; restore default renderer
      (let [value  ~hf/value
            inputs (val (first (::inputs (meta value))))
            args   (->> (p/for [[k v] inputs] [k ~v])
                        (into {}))]
        (let [[k v] (first value)]
          (binding [hf/args   args]
            {k ~v})))))

(tests
  (p/run (binding [hf/render use-input-renderer]
           (! (hfql {(submissions {(needle . ::hf/render needle-renderer
                                             ::hf/options (needle-options))
                                   [:dustingetz/email]})
                     [:dustingetz/email]}) )))
  % := {'(user.gender-shirt-size/submissions needle) [#:dustingetz{:email "alice@example.com"}]})


;; On a traversal
;; Take the right pullexpr
;; and copy it to the options of the LHS, if there are any
;; Options become a traversal
;; But only RHS is returned

(p/defn render-typeahead2 []
  (let [label (::hf/option-label hf/props)]
    [:select {:value ~(label ~hf/value)}
     (p/for [e (apply-by-name (::hf/options hf/props) {'needle ""})]
       [:option (label e)])]))

(tests
  (p/run (! (binding [bypass-renderers false]
              (hfql {(submissions "alice") [{(:dustingetz/gender . ::hf/options (genders)
                                                                 ::hf/render render-typeahead2
                                                                 ::hf/option-label :db/ident)
                                             [(:db/ident . ::hf/as gender)]}
                                            {(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender needle)
                                                                     ::hf/option-label :db/ident
                                                                     ::hf/render  render-typeahead2)
                                             [:db/ident]}]}) )))
  % := '{(user.gender-shirt-size/submissions "alice")
         [#:dustingetz{:gender
                       [:select
                        {:value :dustingetz/female}
                        [[:option :dustingetz/male] [:option :dustingetz/female]]],
                       :shirt-size
                       [:select
                        {:value :dustingetz/womens-large}
                        [[:option :dustingetz/womens-small]
                         [:option :dustingetz/womens-medium]
                         [:option :dustingetz/womens-large]]]}]})



;; (hyperfiddle.rcf/enable! )

#_(tests
  (p/run (! (hfql {(submissions (needle . ::hf/render render-typeahead, ::hf/options (emails needle)))
                   [:dustingetz/email]})))  % := '{(user.gender-shirt-size/submissions needle) [{:dustingetz/email  "alice@example.com"}          {:dustingetz/email  "bob@example.com",}
          {:dustingetz/email  "charlie@example.com",}]})
