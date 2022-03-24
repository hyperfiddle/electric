(ns ^{:clj-kondo/config '{:lint-as {hfdl.lang/def  clojure.core/def
                                    hfdl.lang/defn clojure.core/defn}}}
    hyperfiddle.q6
  (:refer-clojure :exclude [read compile resolve])
  (:require [cljs.analyzer.api :as ana-api]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [hyperfiddle.spec :as spec]
            [missionary.core :as m]
            [user.gender-shirt-size :refer [submissions submission shirt-sizes genders emails sub-profile]]
            [expound.alpha :as expound])
  #?(:cljs (:require-macros [hyperfiddle.api :as hf :refer [render]]
                            [hyperfiddle.q6 :refer [hfql]]))
  )

;; (set! *print-namespace-maps* false)
;; (hyperfiddle.rcf/enable!)
;; (hyperfiddle.rcf/enable! true)
;; (alter-var-root #'hyperfiddle.rcf/*generate-tests* (constantly false))


(do
  (s/def ::literal    any?)
  (s/def ::keyword    qualified-keyword?) ; qualified kws required everywhere but for props.
  (s/def ::symbol     symbol?)
  (s/def ::prop       (s/cat ::key keyword?, ::value any?)) ;; non-qualified kws allowed in props
  (s/def ::props      (s/and seq? (s/cat ::onto (s/alt ::symbol ::symbol, ::edge ::edge ), ::_dot #{'.} ::props (s/* ::prop))))
  (s/def ::arg        (s/or ::traversal ::traversal, ::props ::props, ::symbol ::symbol, ::literal any?))
  (s/def ::call       (s/and seq? ; TODO produces cryptic expounded messages.
                             #(= ::s/invalid (s/conform ::props %))
                             (s/cat ::op ::symbol ::args (s/* ::arg))))
  (s/def ::edge       (s/or ::nav ::keyword, ::props ::props, ::call ::call, ::traversal ::traversal, ::symbol ::symbol))
  (s/def ::edges      (s/coll-of ::edge :kind vector? :min-count 0))
  (s/def ::traversal  (s/and (s/map-of ::edge ::edges :conform-keys true) #(= 1 (count %))))
  (s/def ::hfql       (s/or ::traversal ::traversal, ::call ::call, ::props ::props, ::edge ::edge, ::edges ::edges)))

;;  ensure spec is accurate and uniform (ns kws, fields, homogeneity)

(tests
  (s/conform ::literal 1)                         := 1
  (s/conform ::keyword ::foo)                     := ::foo
  (s/conform ::keyword :foo)                      := ::s/invalid

  (s/conform ::symbol  'a)                        := 'a
  (s/conform ::symbol  'ns/a)                     := 'ns/a

  (s/conform ::prop '(::a 1))                     := {::key ::a, ::value 1}

  (s/conform ::props '(a . ::a 1))                := '{::onto  [::symbol a],
                                                       ::_dot  ., ; to be removed by the parser
                                                       ::props [{::key ::a, ::value 1}]}


  ;; non-namespaced kws allowed
  (s/conform ::props '(a . :a 1))                := '{::onto  _,
                                                      ::_dot  .,
                                                      ::props [{::key :a, ::value 1}]}

  (s/conform ::call '(f x))                       := '{::op f, ::args [[::symbol x]]}

  (s/conform ::call '(f (a . ::a 1)))             := '{::op f, ::args [[::props {::onto [::symbol a]
                                                                                 ::_dot .
                                                                                 ::props [{::key ::a, ::value 1}]}]]}

  (s/conform ::edge  :a)                          := ::s/invalid
  (s/conform ::edge ::a)                          := [::nav ::a]
  (s/conform ::edge '(f x))                       := [::call {::op 'f, ::args [[::symbol 'x]]}]
  (s/conform ::edge '(a . ::a 1))                 := [::props {::onto [::symbol 'a], ::_dot '., ::props [{::key ::a, ::value 1}]}]

  (s/conform ::traversal {::a []})                := ::s/invalid
  (s/conform ::traversal {'a [::b]})              := {[::symbol 'a] [[::nav ::b]]}
  (s/conform ::traversal {::a [::b]})             := {[::nav ::a] [[::nav ::b]]}

  (s/conform ::traversal '{(a . ::a b) [::c]})    := '{[::props {::onto [::symbol a], ::_dot ., ::props [{::key ::a, ::value b}]}]
                                                       [[::nav ::c]]}

  (s/conform ::traversal '{(foo :bar baz) [::c]}) := '{[::call {::op foo, ::args [[::literal :bar] [::symbol baz]]}]
                                                       [[::nav ::c]]}

  (s/conform ::traversal {::a [::b], ::c [::d]})  := ::s/invalid ; more than one entry, order is lost.


  (s/conform ::call '(f {(a . ::a b) [::c]}))     := '{::op   f,
                                                       ::args [[::traversal
                                                                {[::props {::onto  [::symbol a],
                                                                           ::_dot  .,
                                                                           ::props [{::key ::a, ::value b}]}]
                                                                 [[::nav ::c]]}]]}

  ;; FAIL (expected) because `[:b]` is not a valid set of edges. Spec backtracks
  ;; out of traversal and treat it as a literal. Solvable without ADTs?
  (s/conform ::call '(f {(a . ::a 1) [:b]}))
  :<>
  '{::op f,
    ::args [[::traversal {[::props {::onto [::symbol a],
                                    ::_dot .,
                                    ::props [{::key ::a, ::value 1}]}]
                          [[::nav ::b]]}]]}
  )

(defn diagnose* [form & [opts]]
  (expound/expound ::hfql form (merge {:print-specs? false, :theme :figwheel-theme} opts)))

(defn diagnose! [reader-error]
  (diagnose* (::form (ex-data reader-error))))

(defn read [form]
  (let [conformed (s/conform ::hfql form)]
    (if (= ::s/invalid conformed)
      (throw (ex-info (str "Invalid syntax.\n\n" (with-out-str (diagnose* form {:theme :none})))
                      {::type ::invalid-syntax
                       ::form form
                       ::data (s/explain-data ::hfql conformed)}))
      conformed)))

(comment
  (try
    (read '{a [::b]})
    (catch Throwable t
      (diagnose! t)))
  )

(defn ->traversal [through continuations] ^{:type ::traversal} {::through through, ::continuations continuations})
(defn ->call [f args] ^{:type ::call} {::op f, ::args args})
(defn ->symbol [sym]
  (assert (symbol? sym))
  ^{:type ::symbol, ::source sym} {::symbol sym})

(defn tag [node t] (get (meta node) t))
(defn set-tag [node t value] (vary-meta node assoc t value))

(defn parse [[type token]]
  (let [node (case type
               ::traversal  (->traversal (parse (key (first token))) (parse [::edges (val (first token))]))
               ::edges      (with-meta (mapv #(parse [::edge %]) token) {:type ::edges})
               ::edge       (parse token)
               ::props      (vary-meta (parse (::onto token)) merge {::props        (into {} (map (juxt ::key ::value)) (::props token))
                                                                     ::props-order  (mapv ::key (::props token))
                                                                     ::props-source token})
               ::call       (if (= `quote (::op token))
                              (-> (if (= ::symbol (ffirst (::args token)))
                                    (set-tag (parse (first (::args token))) ::quoted true)
                                    (parse (read (second (first (::args token))))))
                                  (set-tag ::quoted true))
                              (->call (parse [::symbol (::op token)])
                                      (with-meta (mapv parse (::args token)) {:type ::args})))
               ::nav        {::keyword token}
               ::symbol     (->symbol token)
               ::literal    {::form token})]
    (if (#{::edge ::props ::call} type)
      node
      (vary-meta node assoc :type type, ::source token))))

(defn ->ast [form] (parse (read form)))

(tests
  (type (->ast '(a . ::a 1))) := ::symbol ; not a fn call
  ;; non-namespaced keywords are exceptionaly allowed in props.
  (type (->ast '(a . :a 1))) := ::symbol)

(defn source-form [[type child]]
  (case type
    ::traversal (source-form (::through child))
    ::props     (::onto child)
    ::literal   (::form child)))

(do
  (derive ::branch     ::node)
  (derive ::leaf       ::node)
  (derive ::traversal  ::branch)
  (derive ::edges      ::branch)
  (derive ::call       ::branch)
  (derive ::args       ::branch)
  (derive ::nav        ::leaf)
  (derive ::symbol     ::leaf)
  (derive ::literal    ::leaf)
  (derive ::vars       ::branch)
  (derive ::var        ::leaf)
  (derive ::proxy      ::branch))

(defn properties [node] (::props (meta node)))

(defn- node-matches? [pred node] (if (set? pred) (pred (type node)) (pred node)))

(defn walk-ast
  ([f ast]
   (condp #(isa? %2 %1) (type ast)
     ::leaf               (f ast)
     ::branch             (let [ast (f ast)]
                            (if (reduced? ast)
                              ast
                              (reduce-kv (fn [r k v] (assoc r k (walk-ast f v))) ast ast)))
     clojure.lang.Reduced @ast))
  ([pred ast f & args]
   (walk-ast (fn [ast] (if (node-matches? pred ast) (apply f ast args) ast)) ast)))

(defn unparse
  ([ast] (unparse #{} ast))
  ([filters ast]
   (let [drop-props? (contains? filters ::drop-props)
         call?       (contains? filters ::call)
         ast         (if drop-props?
                        (walk-ast properties ast vary-meta dissoc ::props-source)
                        ast)]
     (case (type ast)
       ::traversal (let [filters' (disj filters ::call)]
                     (if (and drop-props? call?)
                       (unparse filters' (::through ast))
                       {(unparse filters' (::through ast)) (mapv (partial unparse filters') (::continuations ast))}))
       ::call      (cons (unparse filters (::op ast)) (map (partial unparse (conj filters ::call)) (::args ast)))
       ::args      (mapv (partial unparse filters) ast)
       (let [metas        (meta ast)
             props-source (::props-source metas)]
         (if (and (not drop-props?) (some? props-source))
           (s/unform ::props props-source)
           (case (type ast)
             ::nav (s/unform ::keyword (::keyword ast))
             (s/unform (type ast) (::source metas)))))))))

(defn- roundtrip
  ([form] (roundtrip form false))
  ([form drop-props?] (unparse (if drop-props? #{::drop-props} #{}) (->ast form))))

(tests
  (roundtrip ::a)                           := ::a
  (roundtrip [::a])                         := [::a]
  (roundtrip {::a [::b]})                   := {::a [::b]}
  (roundtrip '(f x))                        := '(f x)

  (roundtrip '(a . ::a 1))                  := '(a . ::a 1)
  (roundtrip '(f (a . ::a 1)))              := '(f (a . ::a 1))
  (roundtrip '(f {(a . ::a 1) [::b]}))      := '(f {(a . ::a 1) [::b]})
  (roundtrip '{(::a . :a 1) [::b]})         := '{(::a . :a 1) [::b]}

  (roundtrip '(a . ::a 1) true)             := 'a
  (roundtrip '(f (a . ::a 1)) true)         := '(f a)
  (roundtrip '(f {(a . ::a 1) [::b]}) true) := '(f a)
  (roundtrip '{(::a . :a 1) [::b]} true)    := '{::a [::b]})

(defn- short-keyword-print [^clojure.lang.Keyword kw, ^java.io.Writer w]
  (if-let [ns-str (.getNamespace kw)]
    (if-let [ns (find-ns (symbol ns-str))]
      (if (= ns *ns*)
        (.write w (str "::" (name kw)))
        (if-let [ns-alias (some #(when (= ns (val %)) (key %)) (ns-aliases *ns*))]
          (.write w (str "::" ns-alias "/" (name kw)))
          (.write w (str ":" ns-str "/" (name kw)))))
      (.write w (str ":" ns-str "/" (name kw))))
    (.write w (str ":" (name kw)))))

(defmacro ^:private with-short-keys [& body]
  `(binding [*print-namespace-maps* false]
     (let [old-kw-method# (get (methods print-method) clojure.lang.Keyword)]
       (. ~(with-meta `print-method {:tag 'clojure.lang.MultiFn})
          addMethod clojure.lang.Keyword short-keyword-print)
       (try
         ~@body
         (finally
           (. ~(with-meta `print-method {:tag 'clojure.lang.MultiFn})
              addMethod clojure.lang.Keyword old-kw-method#))))))

(defmethod print-method ::node [v ^java.io.Writer w]
  (.write w (with-short-keys (str "#" (pr-str (type v)) " " (pr-str (if  (s/get-spec (type v))
                                                                      (unparse v)
                                                                      (vary-meta v dissoc :type)))))))

(defn reduce-ast
  ([rf r ast] (reduce-ast rf r [] ast))
  ([rf r path ast]
   (condp #(isa? %2 %1) (type ast)
     ::leaf   (rf r path ast)
     ::branch (reduce-kv (fn [r k v] (reduce-ast rf r (conj path k) v)) (rf r path ast) ast))))

(defn ast-seq [pred ast]
  (sequence (reduce-ast (fn [r _ ast] (if (node-matches? pred ast) (conj r ast) r)) [] ast)))

(tests
  "AST structure is preserved"
  (walk-ast identity (->ast '{::a [::b]})) := (->ast '{::a [::b]})
  (walk-ast identity (->ast '{::a [{::b [::c]}]})) := (->ast '{::a [{::b [::c]}]})
  (walk-ast #{::nav} (->ast '{::a [{::b [::c]}]}) identity) := (->ast '{::a [{::b [::c]}]})


  "same order as tree-seq"
  (ast-seq (constantly true) (->ast '{::a [{::b [::c]}]}))
  := (filter #(isa? (type %) ::node)
             (tree-seq coll? identity (->ast '{::a [{::b [::c]}]})))

  (ast-seq #{::nav} (->ast '{::a [{::b [::c]}]})) := (list (->ast ::a) (->ast ::b) (->ast ::c))
  )

;;;;;;;;;;;;
;; PASSES ;;
;;;;;;;;;;;;

(defn resolve-ns-alias [env sym]
  (let [ns  (namespace sym)
        nom (name sym)]
    (when-let [qualified (get (::aliases env) ns)]
      (symbol qualified nom))))

(defn resolve-global [env x]
  (assert (symbol? x))
  (if (:js-globals env)
    (:name (ana-api/resolve env x))
    (or (resolve-ns-alias env x)
        (let [?var (get (::globals env) x)]
          (if (var? ?var)
            (symbol ?var)
            (when (qualified-symbol? x)
              x))))))

(defn resolve-local [env x] (get env x))

(defn resolvable? [env x] (or (resolve-local env x) (resolve-global env x)))

(defn define-alias [env form aliased-to] (assoc env [::alias form] aliased-to))
(defn unalias      [env form] (dissoc env [::alias form]))
(defn alias [env form] (get env [::alias form]))
(defn aliased? [env form] (some? (alias env form)))

(defn resolve [sym env]
  (if-not (aliased? env sym)
    (or (resolve-global env sym) sym)
    sym))

(defn- qualify-syms-in-source [source env]
  (walk/prewalk #(resolve % env) source))

(defn property
  ([prop] (fn [node] (property prop node)))
  ([prop node] (get-in (meta node) [::props prop])))

(defn set-property [node k v] (vary-meta node assoc-in [::props k] v))

(defn maybe-update [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))

(defn maybe-update-in [m ks f & args]
  (if (and (seq (butlast ks)) (contains? (get-in m (butlast ks)) (last ks)))
    (apply update-in m ks f args)
    m))

(defn update-property [node prop f & args] (apply vary-meta node maybe-update-in [::props prop] f args))

(defn gather-aliases-pass [env ast]
  (let [env (->> (ast-seq (property ::hf/as) ast)
                 (map (property ::hf/as))
                 (reduce (fn [env as] (define-alias env as as))
                         env))]
    [env ast]))

(defn qualify-syms-pass
  "Expand all symbols to their fully qualified names, accounting for ns aliases."
  [env ast]
  (let [resolvef (fn [ast] (walk-ast #{::symbol} ast #(-> (update % ::symbol resolve env)
                                                          (vary-meta update ::source qualify-syms-in-source env))))]
    [env (as-> ast ast
           (resolvef ast)
           (walk-ast (property ::hf/options) ast update-property ::hf/options resolvef))]))


(defn local-binding? [v] (instance? clojure.lang.Compiler$LocalBinding v))

(defn make-env [&env]
  (let [env (into {} (reduce-kv (fn [r k v] (if (local-binding? v)
                                              (assoc r (.-sym v) (.-sym v))
                                              (assoc r k v))) {} &env))]
    (-> (assoc env ::globals (ns-map *ns*))
        (assoc ::aliases (reduce-kv (fn [r k v] (assoc r (str k) (str v))) {} (ns-aliases *ns*)))
        (vary-meta assoc :type ::env))))

(defmethod print-method ::env [v ^java.io.Writer w]
  (.write w (str "#::env " (-> (dissoc v ::globals) (vary-meta dissoc :type) (pr-str)))))

(defn- apply-passes
  ([passes ast] (apply-passes passes (make-env {}) ast))
  ([passes env ast]
   (reduce (fn [r pass] (apply pass r))
           [env ast]
           passes)))

(tests
  (unparse (second (apply-passes [qualify-syms-pass] (->ast '(inc a))) )) := '(clojure.core/inc a))

(defn parse-hf-props-pass [env ast]
  [env (as-> ast ast
         (walk-ast (property ::hf/options) ast update-property ::hf/options ->ast)
         (walk-ast (property ::hf/as) ast update-property ::hf/as ->symbol))])

(defn props-on-continuations-pass [env ast]
  [env (walk-ast #{::traversal} ast
                 (fn [{:keys [::through] :as traversal}]
                   (update traversal ::continuations vary-meta merge (select-keys (meta through) #{::props ::props-order ::props-source}))) )])

;; TODO support passing a symbol to ::hf/options (only fn calls allowed for now)
;; TEST maybe it works oob, to be tested.
(defn continuations-on-hf-options-pass [env ast]
  [env (walk-ast #{::traversal} ast
                 (fn [{:keys [::through ::continuations] :as traversal}]
                   (update traversal ::through update-property ::hf/options ->traversal continuations)))])

(defn ->free-var [path ast]
  (with-meta {::symbol (::symbol ast)
              ::alias  (symbol (str (::symbol ast) "_form"))
              ::path   path}
    (assoc (meta ast) :type ::var)))

(defn- forks-here? [path1 path2]
  (and (= (first path1) (first path2))
       (not= (second path1) (second path2))))

(defn- dig [path point] (conj path (hash point)))

(defn free-variable? [env ast]
  (assert (= ::symbol (type ast)))
  (let [sym (::symbol ast)]
    (when (or (aliased? env sym) ; aliases shadows local and global env
              (not (resolvable? env sym)))
      sym)))

;; FIXME should not hoist any var outside of a cardinality-many traversal.
(defn free-variables-here
  ([env ast] (let [vars           (free-variables-here [] env ast) ; find all free variables from this point
                   ;; declared-here  (filter #(= 1 (count (::path %))) vars) ; keep the ones immediately at the root (here)
                   ;; vars           (remove (set declared-here) vars)       ; focus on deeper ones
                   diverging-here (for [v     vars ; keep deeper vars that start to diverge here
                                        w     vars
                                        :when (and (not= v w)
                                                   (= (::symbol v) (::symbol w))
                                                   (forks-here? (::path v) (::path w)))]
                                    v)]
               (concat #_declared-here diverging-here)))
  ([path env ast]
   (set/union
    (if (= ::edges (type ast))
      #{} ;; props on edges are artificial
      (reduce-kv (fn [r k v] (if (isa? (type v) ::node)
                               (into r (map #(assoc % ::origin k))
                                     (free-variables-here (dig path ast) env v))
                               r))
                 #{} (properties ast)))
    (case (type ast)
      ::symbol    (if (free-variable? env ast) #{(->free-var path ast)} #{})
      ::call      (->> (free-variables-here path env (::args ast))
                       (map #(assoc % ::path path ::origin ::call)) ; hoist the arg declaration at the fncall level.
                       )
      ::args      (->> (map (partial free-variables-here path env) ast)
                       (apply set/union))
      ::edges     (->> (map (partial free-variables-here (dig path ast) env) ast)
                       (apply set/union))
      ::traversal (set/union (free-variables-here (-> (dig path ast) (dig (::through ast))) env (::through ast))
                             (free-variables-here (-> (dig path ast) (dig (::through ast))) env (::continuations ast)))
      #{}))))

(defn difference-by [f xs ys]
  (let [exclusions (set (map f ys))]
    (set (remove (fn [x] (exclusions (f x))) xs))))

(tests (difference-by identity #{1 2} #{2}) := #{1})

(defn collect [ks group]
  (conj ((apply juxt ks) group) (apply concat (vals (apply dissoc group ks)))))

(defn pick-vars-introducing-bindings [vars]
  (let [[aliases others] (mapv set (collect [::hf/as] (group-by ::origin vars)))]
    (->> (concat aliases (difference-by ::symbol others aliases))
         (sort #(compare (.indexOf vars %1) (.indexOf vars %2))))))

(defn toposort-free-variables-pass
  "Symbols that can not be resolved from the parent env are considered free variables.
  The first occurrence introduces a lexical binding at the closest common
  ancestor of all instances of the free variable. Further references refers to the lexical binding."
  [env ast]
  (let [!visited (atom #{})]
    [env (walk-ast #{::traversal ::edges} ast
                   (fn [ast]
                     (if (contains? @!visited (hash ast))
                       ast
                       (if-some [vars (-> (free-variables-here env ast)
                                          (pick-vars-introducing-bindings)
                                          (seq))]
                         (do (swap! !visited conj (hash ast))
                             (with-meta {::let (with-meta (vec vars) {:type ::branch})
                                         ::in  ast}
                               {:type ::vars}))
                         ast))))]))

(defn find-by-hash [positionf ast hashes]
  (->> (ast-seq (constantly true) ast)
       (map (juxt hash identity))
       (filter (comp (set hashes) first))
       (remove (comp #{::edges} type second))
       (positionf)
       (second)))

(defn path-of [node ast]
  (cond
    (nil? ast)   nil ; no path to node
    (= node ast) (case (type ast)
                   ::traversal (list (unparse #{::drop-props} (::through ast)))
                   ::edges     (throw (ex-info "Malformed path" {}))
                   (list (unparse #{::drop-props} ast)))
    :else        (case (type ast)
                   ::traversal (cons (unparse #{::drop-props} (::through ast)) (path-of node (::continuations ast)))
                   ::edges     (first (filter identity (map (partial path-of node) ast)))
                   nil)))

(defn lazy-getter [sym path]
  ;; TODO bind hf/render to join-1 to only sample path (do not sample all the map, just navigate through it)
  `(binding [render-mode ::edn
             hf/render hf/sequenceM]
     #_(apply list '-> sym `(unquote) (interpose `(unquote) (map #(list 'get %) path)))
     (get-in (unquote ~sym) ~(vec path))))

(defn ->proxy [alias to] ^{:type ::proxy} {::alias alias, ::to to})

(defn move-ast-to-free-var-binding-pass [env ast]
  (let [!env (atom env)
        ast  (walk-ast #{::vars} ast
                       (fn [vars]
                         (let [new-vars (into (empty (::let vars))
                                              (mapcat (fn [var]
                                                        (let [ast   (find-by-hash first (::in vars) (::path var))
                                                              point (find-by-hash last (::in vars) (::path var))
                                                              path  (path-of point ast)]
                                                          (case (::origin var)
                                                            ::hf/as (do (swap! !env define-alias (hash ast) `(get (p/$ hf/flatten-1 ~(::alias var)) ~(first path)))
                                                                        ;; (swap! !env define-alias (::symbol var) `(get (unquote ~(::symbol var)) (first path)))
                                                                        [(assoc var ::body ast)
                                                                         (assoc var ::alias (::symbol var),
                                                                                ::body (->proxy (::alias var) path))])
                                                            (do (swap! !env define-alias (::symbol var) (list `unquote (::symbol var)))
                                                                [(assoc var ::sym (::alias var) ::body ast)]))))
                                                      (::let vars)))]
                           (assoc vars ::let new-vars))))]
    [@!env ast]))

(defn ->literal [form]
  ^{:type ::literal, ::source form} {::form form})

(defn expand-aliases-pass [env ast]
  (let [expandf (fn [ast]
                  (walk-ast #{::symbol} ast
                            (fn [ast]
                              (if (aliased? env ast)
                                (set-tag (->literal `(unquote ~(::symbol ast))) ::const-input true)
                                ast))))]
    [env (as-> ast ast
           (expandf ast)
           (walk-ast (property ::hf/options) ast update-property ::hf/options expandf))]))

(defn rewrite-remaining-free-vars-pass [env ast]
  (let [rewritef (fn [ast]
                   (walk-ast #{::args} ast
                             (fn [ast]
                               (walk-ast #{::symbol} ast (fn [{:keys [::symbol] :as ast}] (if (resolvable? env symbol)
                                                                                            ast
                                                                                            (set-tag ast ::resolve-to ::hf/args)))))))]
    [env (as-> ast ast
           (rewritef ast)
           ;; TODO walk-ast should just walk the props too.
           (walk-ast (property ::hf/options) ast update-property ::hf/options rewritef))]))

(defn rewrite-symbol-edge-pass [env ast]
  [env (walk-ast #{::edges} ast (fn [edges]
                                  (into (empty edges) (map (fn [edge]
                                                             (case (type edge)
                                                               ::symbol (cond-> edge
                                                                          (tag edge ::quoted) (set-property ::hf/render `hf/link)
                                                                          true                (->traversal ^{:type ::edges} []))
                                                               edge))) edges)))])

(defn escape [form]
  (if (or (symbol? form) (seq? form))
    (list 'quote form)
    form))

(defn unescape [form]
  (if (and (seq? form)
           (or (= `quote (first form))
               (= `unquote (first form))))
    (unescape (second form))
    form))

(defn key-of [ast]
  (case (type ast)
    ::traversal (key-of (::through ast))
    (escape (unparse #{::drop-props} ast))))

(defn keys-of [ast]
  (case (type ast)
    ::vars      (keys-of (::in ast))
    ::edges     (mapv key-of ast)
    ::traversal [(key-of ast)]))

(defn clean-props [props]
  (reduce-kv (fn [r k v] (case k
                           ::hf/as      (assoc r k (escape (unparse #{::drop-props} v)))
                           ::hf/default (assoc r k `'~v)
                           ::hf/render  (assoc r k (if (symbol? v) v `(var ~v)))
                           r)) props props))

;; When ::edn, bypasses custom renderers at runtime.
;; Use case: get the value of an aliased form as intended by the pull expression.
(p/def render-mode ::user) ;; #{::edn ::default ::user}

(p/defn render [user-renderer]
  ;; (prn "render-mode" render-mode)
  (case render-mode
    ::default ~hf/render
    ::edn     ~hf/sequenceM
    ::user    ~user-renderer))

(defn emit-renderer [ast]
  (let [renderer (or (::hf/render (properties ast)) `hf/render)]
    (if (symbol? renderer)
      `(p/$ render ~renderer)
      `(p/$ render #'(do ~renderer)))))

(declare emit)

(defn emit-traversal-call [env ast]
  (case (type (::through ast))
    ::call   (emit env (::through ast))
    ::nav    `(unquote (hf/nav hf/entity ~(unparse #{::drop-props} (::through ast))))
    ::symbol (let [symbol (unparse #{::drop-props} (::through ast))]
               (cond
                 (tag (::through ast) ::quoted) `(list '~symbol hf/entity)
                 (simple-symbol? symbol)        (emit env (::through ast))
                 :else                          `(p/$ ~(unparse #{::drop-props} (::through ast)))))))

(defn map-entry [k v] (first {k v}))

(defn collect-inputs [ast]
  (concat #_(when-let [options (property ::hf/options ast)] (collect-inputs options))
          (case (type ast)
            ::call      (into [] (filter #(or (#{::traversal ::symbol} (type %))
                                              (tag % ::const-input))) (::args ast))
            ::traversal (collect-inputs (::through ast))
            ;; ::symbol    (list ast)
            nil)))

(defn emit-props
  ([env ast]
   (-> (properties ast)
       (maybe-update ::hf/options (fn [options] (list `var (emit env options #{::options}))))
       clean-props))
  ([input-sym env ast]
   (-> (properties ast)
       (maybe-update ::hf/options (fn [options] (list `var (emit env options #{::options}))))
       ;; (maybe-update ::hf/default (fn [defaults] `(let [~input-sym (get hf/args '~input-sym)] ~defaults)))
       ;; (set/rename-keys {::hf/default ::hf/render})
       clean-props
       (assoc ::hf/value `(var ~(emit env ast)))
       (update ::hf/disabled (fn [disabled?] (if (some? disabled?) disabled? (tag ast ::const-input)))))))

(defn emit-inputs [env inputs]
  (mapv (fn [ast] (let [input-sym (key-of ast)]
                    (map-entry (escape (unescape input-sym)) (emit-props input-sym env ast))))
        inputs))

;; DEPRECATED
(defn traversal-cardinality [env ast]
  (let [form (case (type (::through ast))
               ::call           (unparse #{::drop-props} (::op (::through ast)))
               (::nav ::symbol) (unparse #{::drop-props} (::through ast)))]
    (if (or (tag (::through ast) ::quoted) ;; quoted form
            (simple-symbol? form))
      ::spec/one
      (if-some [cardinality (spec/cardinality form)]
        cardinality
        (throw (ex-info (str "Can’t infer cardinality. Please provide a spec for `" form "`") {}))))))

(defn schema-attr [db ?a]
  (condp = (type db)
    datascript.db.DB (get (:schema db) ?a)))

(defn db-cardinality [attr]
  (case (:db/cardinality attr)
    :db.cardinality/one ::spec/one
    :db.cardinality/many ::spec/many
    nil))

(defn cardinality* [db form]
  (let [form (cond
               (qualified-keyword? form)                          form
               (and (seq? form) (qualified-symbol? (first form))) (first form)
               :else                                              (throw (ex-info (str "Can’t infer cardinality of `" form "`.") {})))]
    (or (spec/cardinality form)
        (db-cardinality (schema-attr db form))
        (throw (ex-info (str "Can’t infer cardinality. Please provide a spec for `" form "`") {})))))

(p/defn cardinality [db form] (cardinality* db form))

(p/def renderer hyperfiddle.api/render)

;; TODO support dynamic cardinality. Need to query datomic schema.
(defn emit-traversal [env ast opts]
  (let [local-sym?     (and (= ::symbol (type (::through ast)))
                            (simple-symbol? (::symbol (::through ast))))
        options?       (some #{::options} opts)
        flatten?       (or local-sym? (some #{::edges ::options} opts))
        call           (emit-traversal-call env ast)
        columns        (if flatten? (keys-of (::continuations ast)) (keys-of ast))
        inputs         (if false #_(some #{::edges} opts) [] (emit-inputs env (collect-inputs ast)))
        props          (if flatten? (emit-props env (::through ast)) (emit-props env ast))
        card-many-body `(binding [hf/db    "$"
                                  hf/value (var (p/for [~'% ~call]
                                                  (var (binding [hf/db     "$"
                                                                 hf/entity ~'%]
                                                         (unquote ~'continuation)))))
                                  renderer hf/render]
                          ~(emit-renderer (::through ast)))
        card-one-body  `(binding [hf/db     "$"
                                  hf/entity ~call
                                  renderer  (var ~(emit-renderer (::through ast)))]
                          (unquote ~'continuation))
        continuation   `(binding [hf/columns           ~(keys-of (::continuations ast))
                                  hf/inputs            ~inputs
                                  hf/props             ~props
                                  ;; hf/entity    ~'entity
                                  ~@(if options? `[hf/options-attribute hf/attribute] [])
                                  hf/attribute         ~(key-of ast)]
                          (let [~'continuation (var ~(emit env (::continuations ast) #{::traversal}))]
                            ~(if local-sym?
                               card-one-body
                               `(case #_(p/$ cardinality hf/*$* ~(key-of ast)) ~(traversal-cardinality env ast) ;; TODO make it dynamic
                                      ::spec/many                              ~card-many-body
                                      ::spec/one                               ~card-one-body))))]
    (if flatten?
      continuation
      `(binding [hf/columns ~columns
                 hf/props   ~props
                 hf/value   (var {~(key-of (::through ast)) (var ~continuation)})]
         (unquote hf/render)))))

(defn emit [env ast & [opts]]
  (or (alias env (hash ast))
      (case (type ast)
        ::nav       `(binding [hf/props     ~(emit-props env ast)
                               ;; hf/entity    ~'entity
                               hf/attribute ~(key-of ast)
                               hf/value     (var (unquote (hf/nav hf/entity ~(::keyword ast))))]
                       ~(emit-renderer ast))
        ::edges     `(binding [hf/columns ~(keys-of ast)
                               hf/props   ~(emit-props env ast)
                               ;; hf/entity  ~'entity
                               hf/value   (var ~(if (seq ast)
                                                  (zipmap (keys-of ast) (map #(list `var (emit env % #{::edges})) ast))
                                                  `hf/entity))]
                       ~(if (some #{::traversal} opts)
                          `(unquote renderer)
                          (emit-renderer ast)))
        ::traversal (if (some #{::call} opts)
                      (emit env (::through ast))
                      (emit-traversal env ast opts))
        ::call      (if (tag ast ::quoted)
                      `(list (quote ~(unparse #{::drop-props} (::op ast)))
                             ~@(map #(emit env % #{::call}) (::args ast)))
                      (let [call (apply list `p/$ (unparse #{::drop-props} (::op ast))
                                        (map #(emit env % #{::call}) (::args ast)))]
                        (if (some #{::options} opts)
                          `(binding [hf/render hf/sequenceM] ~call)
                          call)))
        ::vars      `(let [~@(mapcat (fn [{:keys [::alias ::body]}]
                                       [alias (list `var (-> (unalias env (hash body))
                                                             (emit body)))])
                                     (::let ast))]
                       ~(emit env (::in ast)))
        ::proxy     (lazy-getter (::alias ast) (::to ast))
        ::symbol    (let [sym  (unparse #{::drop-props} ast)
                          body (case (tag ast ::resolve-to)
                                 ::hf/args `(get hf/args '~sym)
                                 sym)]
                      (if-let [defaults (property ::hf/default ast)]
                        (if (= sym body)
                          defaults
                          `(let [~sym ~body] ~defaults))
                        body))
        ::literal   (unparse ast))))

(defn- compile* [env expr]
  (let [ast       (->ast expr)
        [env ast] (apply-passes [parse-hf-props-pass
                                 gather-aliases-pass
                                 qualify-syms-pass
                                 rewrite-symbol-edge-pass
                                 continuations-on-hf-options-pass
                                 props-on-continuations-pass
                                 toposort-free-variables-pass
                                 expand-aliases-pass
                                 move-ast-to-free-var-binding-pass
                                 rewrite-remaining-free-vars-pass
                                 ]
                                env ast)]
    [ast env (emit env ast)]))

(defn compile [env expr] (last (compile* env expr)))

(defn- hfql*-impl [env expr] (compile env expr))

(defmacro hfql* [env expr] ;; for debug
  (hfql*-impl (merge (make-env &env) env) expr))

(defmacro hfql [expr] (hfql*-impl (make-env &env) expr))

(defmacro ^:private hfql' [expr] ;; for debug
  (list 'quote (compile* (make-env &env) expr)))

(tests
 (compile (make-env {}) :db/id) := '(clojure.core/binding [hyperfiddle.api/props     nil
                                                           hyperfiddle.api/attribute :db/id
                                                           hyperfiddle.api/value     #'~(hyperfiddle.api/nav hyperfiddle.api/entity :db/id)]
                                      (hfdl.lang/$ hyperfiddle.q6/render hyperfiddle.api/render)))

(tests
  (p/run (! (binding [hf/entity 14] (hfql [:db/id]) )))
  % := {:db/id 14})

(comment 
  (p/main
   (hfql :db/id))

  (tests % := 1 % := 2) 
  )


(p/def string-renderer #'(str ~hf/value))

(tests
  "hf/render"
  (p/run (! (binding [hf/entity 14] (hfql (:db/id . ::hf/render string-renderer)) )))
  % := "14")

(tests
  "hf/render inline"
  (p/run (! (binding [hf/entity 14] (hfql (:db/id . ::hf/render (str ~hf/value))) )))
  % := "14")

(tests
  (p/run (! (binding [hf/entity 14] (hfql [(:db/id . ::hf/render string-renderer)]) )))
  % := {:db/id "14"})

(tests
  (p/run (binding [hf/entity 14] (! (hfql {:dustingetz/gender [:db/ident]}) )))
  % := {:dustingetz/gender {:db/ident :dustingetz/female}})

(tests
  (p/run (binding [hf/entity 14] (! (hfql [{:dustingetz/gender [:db/ident]}]) )))
  % := {:dustingetz/gender {:db/ident :dustingetz/female}})

(tests
  (p/run (! (hfql {(submission "") [:db/id]}) ))
  % := {'(user.gender-shirt-size/submission "") {:db/id 14}})

(tests
  "EAV"
  (p/run (! (hfql {(submission "") [(:dustingetz/email . ::hf/render [hf/entity hf/attribute ~hf/value])]}) ))
  % := '{(user.gender-shirt-size/submission "") {:dustingetz/email [14 :dustingetz/email "alice@example.com"]}})

(tests
  (p/run (! (hfql {(submission "") [{:dustingetz/shirt-size [:db/ident]}]}) ))
  % := {'(user.gender-shirt-size/submission "") #:dustingetz{:shirt-size #:db{:ident :dustingetz/womens-large}}})

(tests
  (p/run (! (hfql {(submissions "") [:db/id]}) ))
  % := {'(user.gender-shirt-size/submissions "") [{:db/id 14} {:db/id 15} {:db/id 16}]})

(tests
  (p/run (! (hfql {(submissions "") [(:db/id . ::hf/render string-renderer)]}) ))
  % := {'(user.gender-shirt-size/submissions "") [{:db/id "14"} {:db/id "15"} {:db/id "16"}]})

(defn fail []
  (throw (ex-info "I fail" {})))

(p/def throwing-renderer #'(fail))

(p/def ignoring-renderer #'"ignored")

(tests
  (p/run (! (binding [hf/entity 14]
              (hfql {(:dustingetz/gender . ::hf/render ignoring-renderer)
                     [(:db/ident . ::hf/render throwing-renderer)]}) )))
  % := #:dustingetz{:gender "ignored"} ; note it didn’t throw
  )

;;;;;;;;;;;;;
;; OPTIONS ;;
;;;;;;;;;;;;;


(p/defn join-all [>v]
  (binding [hf/value >v
            hf/render hf/sequenceM]
    ~hf/render))

(p/def select-option-renderer
  #'(into [:select {:value (p/$ join-all hf/value)}]
          (p/for [e ~(::hf/options hf/props)]
            [:option e])))

(tests
  (p/run (! (binding [hf/entity 14]
              (hfql (:dustingetz/shirt-size . ::hf/render select-option-renderer
                                              ::hf/options (shirt-sizes :dustingetz/female "")
                                              ))
              )))
  %
  := [:select {:value 13} [:option 11] [:option 12] [:option 13]]
  )

(tests
  (p/run (! (binding [hf/entity 14]
              (hfql {(:dustingetz/shirt-size . ::hf/render select-option-renderer
                                             ::hf/options (shirt-sizes :dustingetz/female ""))
                     [:db/ident]}) )))
  % := {:dustingetz/shirt-size [:select {:value #:db{:ident :dustingetz/womens-large}}
                                [:option #:db{:ident :dustingetz/womens-small}]
                                [:option #:db{:ident :dustingetz/womens-medium}]
                                [:option #:db{:ident :dustingetz/womens-large}]]})

;;;;;;;;;
;; ENV ;;
;;;;;;;;;

#_(first (compile* (make-env {}) '[(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender ""))
                                 {:dustingetz/gender [(:db/ident . ::hf/as gender)]}]))

;; HERE
(tests
  (p/run (binding [hf/entity 14]
           (! (hfql [(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender ""))
                     {:dustingetz/gender [(:db/ident . ::hf/as gender)]}])
              )))
  % := #:dustingetz{:gender     #:db{:ident :dustingetz/female},
                    :shirt-size 13})


(p/defn shirt-sizes-renderer []
  ~(::hf/options hf/props))

(tests
  (p/run (binding [hf/entity 14]
           (! (hfql [{:dustingetz/gender [(:db/ident . ::hf/as gender)]}
                     {(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender "")
                                              ::hf/render shirt-sizes-renderer)
                      [:db/ident]}]) )))
  % := {:dustingetz/gender     {:db/ident :dustingetz/female},
        :dustingetz/shirt-size [{:db/ident :dustingetz/womens-small}
                                {:db/ident :dustingetz/womens-medium}
                                {:db/ident :dustingetz/womens-large}]})

(tests
  "needle resolves from lexical env"
  (let [needle "alice"]
    (p/run (! (hfql {(submissions needle) [:db/id]})
              )))
  % := '{(user.gender-shirt-size/submissions needle) [#:db{:id 14}]})

(p/defn render-typeahead []
  [:select {:value (p/$ join-all hf/value)}
   (p/for [e (binding [hf/args {'needle ""}] ~(::hf/options hf/props))]
     [:option ~(hf/nav e :db/ident)])])

(tests
  (p/run (! (hfql {(submissions needle)
                   [:db/id
                    :dustingetz/email
                    {(:dustingetz/shirt-size . ::hf/render render-typeahead
                                             ::hf/options (shirt-sizes gender needle))
                     [:db/ident]}
                    {(:dustingetz/gender . ::a 1) [(:db/ident . ::hf/as gender)]}]}) ))
  % := '{(user.gender-shirt-size/submissions needle)
         [{:dustingetz/gender #:db{:ident :dustingetz/female},
           :dustingetz/email  "alice@example.com",
           :dustingetz/shirt-size
           [:select
            {:value :dustingetz/womens-large}
            [[:option :dustingetz/womens-small]
             [:option :dustingetz/womens-medium]
             [:option :dustingetz/womens-large]]],
           :db/id             14}
          {:dustingetz/gender #:db{:ident :dustingetz/male},
           :dustingetz/email  "bob@example.com",
           :dustingetz/shirt-size
           [:select
            {:value :dustingetz/mens-large}
            [[:option :dustingetz/mens-small]
             [:option :dustingetz/mens-medium]
             [:option :dustingetz/mens-large]]]
           :db/id             15}
          {:dustingetz/gender #:db{:ident :dustingetz/male},
           :dustingetz/email  "charlie@example.com",
           :dustingetz/shirt-size
           [:select
            {:value :dustingetz/mens-medium}
            [[:option :dustingetz/mens-small]
             [:option :dustingetz/mens-medium]
             [:option :dustingetz/mens-large]]],
           :db/id             16}]})

;;;;;;;;;;;;;;
;; DEFAULTS ;;
;;;;;;;;;;;;;;

(tests
  "Input is defaulted"
  (p/run (! (let [sub 14]
              (hfql {(submissions (sub . ::hf/default ~(hf/nav sub :dustingetz/email))) [:db/id]})
              )))
  % := '{(user.gender-shirt-size/submissions sub) [#:db{:id 14}]})

;; TODO use an ordered map
(defn get-input [associative k] ;; inputs are not in a map but a seq of kvs
  (->> associative
       (sequence (filter #(= k (first %))))
       first
       second))

(p/def render-sub-hydrated-defaults #'~(::hf/value (get-input hf/inputs 'sub)))

(tests
  "Input is hydrated"
  (p/run (! (let [sub 14]
              (hfql* {sub sub} {((submissions {sub [:dustingetz/email]})
                                 . ::hf/render render-sub-hydrated-defaults)
                                [:db/id]})
              )
            ))
  % := '{(user.gender-shirt-size/submissions sub) #:dustingetz{:email "alice@example.com"}})


(p/def render-sub-defaults-and-hydrated-defaults #'{:sub ~(::hf/value (get-input hf/inputs 'sub))
                                                    :result (p/$ join-all hf/value)})

(tests
  "Input is hydrated and defaulted"
  (p/run (! (let [sub [:dustingetz/email "alice@example.com"]]
              (hfql* {sub sub} {((sub-profile {(sub . ::hf/default ~(hf/nav sub :db/id)) [:db/id :dustingetz/email]})
                                 . ::hf/render render-sub-defaults-and-hydrated-defaults)
                                [:db/id]})
              )
            ))
  % := '{(user.gender-shirt-size/sub-profile sub) {:sub {:dustingetz/email "alice@example.com", :db/id 14}, :result #:db{:id 14}}})

;;;;;;;;;;;;;;;;;;;
;; VIEW-DEFAULTS ;;
;;;;;;;;;;;;;;;;;;;

(p/def needle-renderer #'(let [options (binding [hf/render hf/sequenceM
                                                 hf/args   {'needle "alice"}]
                                         ~(::hf/options hf/props))]
                           (:dustingetz/email (first options))))

(p/defn render-input [symbol]
  (binding [hf/props (get-input hf/inputs symbol)]
    ~(::hf/value hf/props)))

(p/def use-needle-renderer #'(binding [hf/args {'needle (p/$ render-input 'needle)}
                                       hf/render hf/sequenceM]
                               (p/$ join-all hf/value)))

(tests
  "Filter submissions with needle options"
  (p/run (! (hfql {((submissions {(needle . ::hf/render needle-renderer
                                          ::hf/options (emails needle))
                                  [:dustingetz/email]})
                    . ::hf/render use-needle-renderer)
                   [:dustingetz/email]})
            ))
  % := '{(user.gender-shirt-size/submissions needle)
         [{:dustingetz/email "alice@example.com"}]})



(tests
  "hf/defaults"
  (p/run (! (hfql {(submissions (needle . ::hf/default (or needle "alice"))) [:dustingetz/email]})
            ))
  % := '{(user.gender-shirt-size/submissions needle) [{:dustingetz/email "alice@example.com"}]}


  (p/run (! (let [needle "bob"]
              (hfql {(submissions (needle . ::hf/default (or needle "alice"))) [:dustingetz/email]}))
            ))
  % := '{(user.gender-shirt-size/submissions needle) [{:dustingetz/email "bob@example.com"}]}
  )

;; DONE GOOD HERE

(s/fdef user-name :ret any?)
(p/def user-name #'(let [email ~(hf/nav hf/entity :dustingetz/email)]
                     (first (str/split email #"@"))))

(tests
  "symbol as a nav"

  (p/run (! (hfql {(submissions "alice") [user-name :dustingetz/email]})
            ))
  % := '{(user.gender-shirt-size/submissions "alice") [{hyperfiddle.q6/user-name "alice"
                                                        :dustingetz/email         "alice@example.com"}]}

  (p/run (! (hfql {(submissions "alice") [(user-name . ::hf/render (str/upper-case ~hf/value)) :dustingetz/email]})
            ))
  % := '{(user.gender-shirt-size/submissions "alice") [{hyperfiddle.q6/user-name "ALICE"
                                                        :dustingetz/email         "alice@example.com"}]}
  )


(s/fdef user-name-with-prefix :ret any?)
(p/defn user-name-with-prefix [prefix] (str prefix " - " ~user-name))

(tests
  "call as a nav"

  (p/run (! (hfql {(submissions "alice") [(user-name-with-prefix "foo") :dustingetz/email]})
            ))
  % := '{(user.gender-shirt-size/submissions "alice") [{(hyperfiddle.q6/user-name-with-prefix "foo") "foo - alice"
                                                        :dustingetz/email                            "alice@example.com"}]}
  )


(tests
  "static link"
  (p/run (! (let [sub "alice"] (hfql {(submissions sub) ['(home)]}) )
            ))
  % := '{(user.gender-shirt-size/submissions sub)
         [{(home) (home)}]}
  )


(tests
  "link as quoted sym"
  (p/run (! (let [sub "alice"] (hfql {(submissions sub) ['sub-profile]}) )
            ))
  % := '{(user.gender-shirt-size/submissions sub)
         [#:user.gender-shirt-size{sub-profile (user.gender-shirt-size/sub-profile 14)}]}
  )

(tests
  "templated link"
  (p/run (! (let [sub "alice"] (hfql {(submissions sub) ['(sub-profile hf/entity sub)]}) )
            ))
  % := '{(user.gender-shirt-size/submissions sub)
         [{(user.gender-shirt-size/sub-profile hyperfiddle.api/entity sub)
           (user.gender-shirt-size/sub-profile 14 "alice")}]}
  )

;; FAIL TODO

#_(tests
  "hf/link prop"
  (p/run (! (let [sub "alice"] (hfql {(submissions sub) [(:db/id . ::hf/link sub-profile)
                                                         (:dustingetz/email . ::hf/link (sub-profile %))]}) )
            ))
  % := '{(user.gender-shirt-size/submissions sub)
         [{:db/id            (hyperfiddle.q6/sub-profile 14)
           :dustingetz/email (hyperfiddle.q6/sub-profile "alice@example.com")}]}
  )



;; TODO List
;; [x] remove ::resolvable
;; [x] write a pass replacing remaining inputs with `(get hf/args 'sym)`
;; [x] add hf/inputs {'sym props}
;; [x] support code in hf/render
;; [x] pass EAV to renderers
;; [x] ::hf/default ?
;; [x]  symbols in edges
;; [x] calls in edges
;; [x] links in edges TODO write a pass transforming `::call (quote a)` into ::link
;; [ ] ::hf/link
;; [ ] dynamic cardinality for datomic attrs

(comment

  (hyperfiddle.rcf/enable!)


  #_(p/def use-input-renderer
    #'(binding [hf/render hf/sequenceM] ; restore default emit-renderer
        (let [value  ~hf/value
              inputs (val (first (::inputs (meta value))))
              args   (->> (p/for [[k v] inputs] [k ~v])
                          (into {}))]
          (let [[k v] (first value)]
            (binding [hf/args args]
              {k ~v})))))

  ;; DONE
  (p/defn render-typeahead2 []
    (let [label (::hf/option-label hf/props)]
      (prn "LABEL" label)
      [:select {:value ~(label ~hf/value)}
       (p/for [e (binding [hf/args {'needle ""}] ~(::hf/options hf/props))]
         [:option (label e)])]))

  ;; Passing but not interesting
  (tests
    (p/run (! (hfql {(submissions "alice") [{(:dustingetz/gender . ::hf/options (genders)
                                                                 ::hf/render render-typeahead2
                                                                 ::hf/option-label :db/ident)
                                             [(:db/ident . ::hf/as gender)]}
                                            {(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender needle)
                                                                     ::hf/option-label :db/ident
                                                                     ::hf/render  render-typeahead2)
                                             [:db/ident]}]}) )) 
    % := '{(user.gender-shirt-size/submissions "alice")
           [{:dustingetz/gender     [:select
                                     {:value :dustingetz/female}
                                     [[:option :dustingetz/male] [:option :dustingetz/female]]],
             :dustingetz/shirt-size [:select
                                     {:value :dustingetz/womens-large}
                                     [[:option :dustingetz/womens-small]
                                      [:option :dustingetz/womens-medium]
                                      [:option :dustingetz/womens-large]]]}]})
  )



;; (hyperfiddle.rcf/enable! )

#_(tests
  (p/run (! (hfql {(submissions (needle . ::hf/render render-typeahead, ::hf/options (emails needle)))
                   [:dustingetz/email]})))  % := '{(user.gender-shirt-size/submissions needle) [{:dustingetz/email  "alice@example.com"}          {:dustingetz/email  "bob@example.com",}
          {:dustingetz/email  "charlie@example.com",}]})
