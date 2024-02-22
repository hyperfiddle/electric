(ns hyperfiddle.electric.impl.lang-de2
  (:refer-clojure :exclude [compile])
  (:require [cljs.analyzer]
            [cljs.env]
            [clojure.string :as str]
            [contrib.assert :as ca]
            [contrib.debug]
            [clojure.set :as set]
            [contrib.triple-store :as ts]
            [dom-top.core :refer [loopr]]
            [fipp.edn]
            [missionary.core :as m]
            [hyperfiddle.electric-de :as-alias e]
            [hyperfiddle.electric.impl.analyzer :as ana]
            [hyperfiddle.electric.impl.cljs-analyzer :as cljs-ana]
            [hyperfiddle.electric.impl.destructure :as dst]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.rcf :as rcf :refer [tests]]))

;;;;;;;;;;;
;;; ENV ;;;
;;;;;;;;;;;

(defn clj-env? [env] (not (contains? env :locals)))
(defn electric-env? [env] (contains? env ::peers))
(defn cljs-env? [env] (and (contains? env :locals) (not (electric-env? env))))
(defn ->env-type [env] (if (:js-globals env) :cljs :clj))
(defn normalize-env [env] (if (clj-env? env) {:locals env, :ns {:name (ns-name *ns*)}} env))
(defn get-ns [env] (-> env :ns :name))

(defn serialized-require [sym]
  ;; we might be expanding clj code before the ns got loaded (during cljs compilation)
  ;; to correctly lookup vars the ns needs to be loaded
  ;; since shadow-cljs compiles in parallel we need to serialize the requires
  (when-not (get (loaded-libs) sym)
    (try (#'clojure.core/serialized-require sym) ; try bc it can be cljs file
         (catch java.io.FileNotFoundException _))))

(let [-base-cljs-env {:context :statement
                      :locals {}
                      :fn-scope []
                      :js-globals (into {}
                                    (map #(vector % {:op :js-var :name % :ns 'js})
                                      '(alert window document console escape unescape
                                         screen location navigator history location
                                         global process require module exports)))}]
  (defn ->cljs-env
    ([] (->cljs-env (ns-name *ns*)))
    ([nssym] (cond-> -base-cljs-env nssym (assoc :ns {:name nssym})))))

;;;;;;;;;;;;;;;;
;;; EXPANDER ;;;
;;;;;;;;;;;;;;;;

(defn- fn-> [f a] (fn [o] (f o a)))

(declare -expand-all-in-try)

(defn macroexpand-clj [o env]
  (serialized-require (ns-name *ns*))
  (if-some [mac (when-some [mac (resolve env (first o))] (when (.isMacro ^clojure.lang.Var mac) mac))]
    (apply mac o env (next o))
    (macroexpand-1 o)))                 ; e.g. (Math/abs 1) will expand to (. Math abs 1)

(def !a (cljs-ana/->!a))

(comment
  (cljs-ana/purge-ns !a 'hyperfiddle.electric-de-test)
  )

(defn expand-macro [env o]
  (let [[f & args] o, n (name f), e (dec (count n))]
    (if (= "." n)
      o
      (if (and (not= ".." n) (= \. (nth n e)))
        `(new ~(symbol (namespace f) (subs n 0 e)) ~@args)
        (if (some? (re-find #"^\.[^.]" n))
          (list* '. (first args) (symbol (subs n 1)) (rest args))
          (if (= :cljs (get (::peers env) (::current env)))
            (if-some [mac (cljs-ana/find-macro-var @!a f (get-ns env))]
              (apply mac o (merge (->cljs-env (get-ns env)) env) args)
              o)
            (macroexpand-clj o env)))))))

(defn find-local-entry [env sym] (contains? (:locals env) sym))
(defn add-local [env sym] (update env :locals assoc sym ::unknown))

(defn ?meta [metao o]
  (if (instance? clojure.lang.IObj o)
    (cond-> o (meta metao) (vary-meta #(merge (meta metao) %)))
    o))

(declare -expand-all)

(defn ?expand-macro [o env caller]
  (if (symbol? (first o))
    (let [o2 (?meta o (expand-macro env o))]
      (if (identical? o o2)
        (?meta o (list* (first o) (mapv (fn-> caller env) (rest o))))
        (caller o2 env)))
    (?meta o (list* (caller (first o) env) (mapv (fn-> caller env) (next o))))))

(defn -expand-all [o env]
  (cond
    (and (seq? o) (seq o))
    (if (find-local-entry env (first o))
      (?meta o (list* (first o) (mapv (fn-> -expand-all env) (rest o))))
      (case (first o)
        ;; (ns ns* deftype* defrecord* var)

        (do) (if (nnext o)
               (let [body (mapv #(list `e/drain %) (next o))
                     body (conj (pop body) (second (peek body)))] ; last arg isn't drained
                 (recur (?meta o (cons `e/amb body)) env))
               (recur (?meta o (second o)) env))

        (let clojure.core/let cljs.core/let)
        (let [[_ bs & body] o] (recur (?meta o (list* 'let* (dst/destructure* bs) body)) env))

        (let*) (let [[_ bs & body] o
                     [bs2 env2] (reduce
                                  (fn [[bs env] [sym v]]
                                    [(conj bs sym (-expand-all v env)) (add-local env sym)])
                                  [[] env]
                                  (partition-all 2 bs))]
                 (?meta o (list 'let* bs2 (-expand-all (?meta body (cons 'do body)) env2))))

        (loop*) (let [[_ bs & body] o
                      [bs2 env2] (reduce
                                   (fn [[bs env] [sym v]]
                                     [(conj bs sym (-expand-all v env)) (add-local env sym)])
                                   [[] env]
                                   (partition-all 2 bs))]
                  (recur (?meta o `(binding [::rec (::ctor (let* [~@(interleave (take-nth 2 bs2) (map #(list ::lookup %) (range)))] ~@body))]
                                     (binding [~@(interleave (range) (take-nth 2 (next bs2)))]
                                       (::call (::lookup ::rec)))))
                    env2))

        (recur) (recur (?meta o `(binding [~@(interleave (range) (next o))]
                                   (::call (::lookup ::rec))))
                  env)

        (case clojure.core/case)
        (let [[_ v & clauses] o
              has-default-clause? (odd? (count clauses))
              clauses2 (cond-> clauses has-default-clause? butlast)
              xpand (fn-> -expand-all env)]
          (?meta o (list* 'case (xpand v)
                     (cond-> (into [] (comp (partition-all 2) (mapcat (fn [[match expr]] [match (xpand expr)])))
                               clauses2)
                       has-default-clause? (conj (xpand (last clauses)))))))

        (if) (let [[_ test then else] o, xpand (fn-> -expand-all env)]
               (?meta o (list 'case (xpand test) '(nil false) (xpand else) (xpand then))))

        (quote) o

        (fn*) (let [[?name more] (if (symbol? (second o)) [(second o) (nnext o)] [nil (next o)])
                    arities (cond-> more (vector? (first more)) list)]
                (?meta o (apply list (into (if ?name ['fn* ?name] ['fn*]) arities))))

        (letfn*) (let [[_ bs & body] o
                       env2 (reduce add-local env (take-nth 2 bs))
                       bs2 (->> bs (into [] (comp (partition-all 2)
                                              (mapcat (fn [[sym v]] [sym (-expand-all v env2)])))))]
                   (?meta o `(let* [~(vec (take-nth 2 bs2)) (::letfn ~bs2)] ~(-expand-all (cons 'do body) env2))))

        (try) (throw (ex-info "try is TODO" {:o o})) #_(list* 'try (mapv (fn-> -all-in-try env) (rest o)))

        (js*) (let [[_ s & args] o, gs (repeatedly (count args) gensym)]
                (recur (?meta o `((fn* ([~@gs] (~'js* ~s ~@gs))) ~@args)) env))

        (binding clojure.core/binding)
        (let [[_ bs & body] o]
          (?meta o (list 'binding (into [] (comp (partition-all 2) (mapcat (fn [[sym v]] [sym (-expand-all v env)]))) bs)
                     (-expand-all (cons 'do body) env))))

        (set!) (recur (?meta o `((fn* [v#] (set! ~(nth o 1) v#)) ~(nth o 2))) env)

        (::site) (?meta o (seq (conj (into [] (take 2) o)
                                 (-expand-all (cons 'do (drop 2 o)) (assoc env ::current (second o))))))

        #_else (?expand-macro o env -expand-all)))

    (instance? cljs.tagged_literals.JSValue o)
    (cljs.tagged_literals.JSValue. (-expand-all (.-val o) env))

    (map-entry? o) (clojure.lang.MapEntry. (-expand-all (key o) env) (-expand-all (val o) env))
    (coll? o) (?meta (meta o) (into (empty o) (map (fn-> -expand-all env)) o))
    :else o))

#_(defn -expand-all-in-try [o env]
    (if (seq? o)
      (if (find-local-entry env (first o))
        (list* (first o) (mapv (fn-> -expand-all env) (rest o)))
        (case (first o)
          (catch) (let [[_ typ sym & body] o, env2 (add-local env sym)]
                    (list* 'catch typ sym (mapv (fn-> -expand-all env2) body)))
          #_else (-expand-all o env)))
      (-expand-all o env)))

(defn expand-all [env o]
  (m/? (cljs-ana/analyze-nsT !a env (get-ns env)))
  (-expand-all o (assoc env ::electric true)))

;;;;;;;;;;;;;;;;
;;; COMPILER ;;;
;;;;;;;;;;;;;;;;

(defn fail!
  ([env msg] (fail! env msg {}))
  ([env msg data] (throw (ex-info (str (when-some [d (::def env)] (str "in " d ":\n")) msg)
                           (merge {:form (-> env ::last pop peek) :in (::def env) :for ((juxt ::me ::current) env)} data)))))

(defn get-them [env] (-> env ::peers keys set (disj (::current env)) first))

(defn cannot-resolve! [env form]
  (fail! env (str "I cannot resolve " "`"form"`"
               (when-let [them (get-them env)]
                 (let [site (name them)]
                   (str ", maybe it's defined only on the " site "?"
                     \newline "If `" form "` is supposed to be a macro, you might need to :refer it in the :require-macros clause."))))
    {:locals (keys (:locals env))}))

(defn ambiguous-resolve! [env sym vs]
  (fail! env
    (str "Unsited symbol `" sym "` resolves to different vars on different peers. Please resolve ambiguity by siting the expression using it.")
    {:resolves vs}))

(defn ns-qualify [node] (if (namespace node) node (symbol (str *ns*) (str node))))

(tests
  (ns-qualify 'foo) := `foo
  (ns-qualify 'a/b) := 'a/b)

(defn qualify-sym-in-var-node "If ast node is `:var`, update :form to be a fully qualified symbol" [env ast]
  (if (and (= :var (:op ast)) (not (-> ast :env :def-var)))
    (assoc ast :form (case (or (get (::peers env) (::current env)) (->env-type env))
                       :clj  (symbol (str (:ns (:meta ast))) (str (:name (:meta ast))))
                       :cljs (:name (:info ast))))
    ast))

(defn closure
  "Analyze a cc/fn form, looking for electric defs and electric lexical bindings references.
  Rewrites the cc/fn form into a closure over electric dynamic and lexical scopes.
  Return a pair [closure form, references to close over].

  e.g.:
  (let [x 1]
    (binding [y 2]
      (fn [arg] [x y arg])))

   =>
  [(fn [x123 y123]
     (fn [& rest-args123]
       (binding [y y123]
         (let [x x123]
           (apply (fn [arg] [x y arg]) rest-args123)))))
   [x y]]
  "
  [env form]
  (let [refered-evars   (atom {})
        refered-lexical (atom {})
        edef?           (fn [ast] (or (#{::node ::node-signifier} (-> ast :meta ::type))
                                    (#{::node ::node-signifier} (-> ast :info :meta ::type))))
        dynamic?        (fn [ast] (or (:assignable? ast) ; clj
                                    (:dynamic (:meta (:info ast))) ; cljs
                                    ))
        lexical?        (fn [ast] (or (::provided? ast) ; clj
                                    (::provided? (:info ast)) ;cljs
                                    ))
        namespaced?     (fn [ast] (qualified-symbol? (:form ast)))
        safe-let-name   (fn [sym] (if (qualified-symbol? sym)
                                    (symbol (str/replace (str (munge sym)) #"\." "_"))
                                    sym))
        record-lexical! (fn [{:keys [form]}]
                          (swap! refered-lexical assoc form
                            (gensym (name form))))
        record-edef!    (fn [{:keys [form] :as ast}]
                          (if (dynamic? ast)
                            (swap! refered-evars assoc form #_(ana/var-name ast) (gensym (name form)))
                            (record-lexical! ast)))
        env             (update env :locals update-vals #(if (map? %) (assoc % ::provided? true) {::provided? true}))
        rewrite-ast     (fn [ast]
                          (cond
                            (edef? ast)    (do (record-edef! ast)
                                               (cond (dynamic? ast)    (qualify-sym-in-var-node env ast)
                                                     (namespaced? ast) (update ast :form safe-let-name)
                                                     :else             ast))
                            (lexical? ast) (do (record-lexical! ast) ast)
                            :else          (qualify-sym-in-var-node env ast)))
        form            (case (or (get (::peers env) (::current env)) (->env-type env))
                          :clj  (-> (ana/analyze-clj (update env :ns :name) form)
                                  (ana/walk-clj rewrite-ast)
                                  (ana/emit-clj))
                          :cljs (-> (binding [cljs.analyzer/*cljs-warning-handlers*
                                              [(fn [_warning-type _env _extra])]]
                                      (ana/analyze-cljs env form))
                                  (ana/walk-cljs rewrite-ast)
                                  (ana/emit-cljs)))
        rest-args-sym   (gensym "rest-args")
        all-syms        (merge @refered-evars @refered-lexical)
        [syms gensyms]  [(keys all-syms) (vals all-syms)]
        fn?             (and (seq? form) (#{'fn 'fn* 'clojure.core/fn 'clojure.core/fn* 'cljs.core/fn 'cljs.core/fn*} (first form)))
        form            (if fn?
                          `(apply ~form ~rest-args-sym)
                          form)
        form            (if (seq @refered-lexical)
                          `(let [~@(flatten (map (fn [[k v]] [(safe-let-name k) v]) @refered-lexical))]
                             ~form)
                          form)
        form            (if (seq @refered-evars)
                          `(binding [~@(flatten (seq @refered-evars))]
                             ~form)
                          form)
        form            (if fn?
                          `(fn [~@gensyms] (fn [~'& ~rest-args-sym] ~form))
                          `(fn [~@gensyms] ~form))]
    [form syms]))

(defn bound-js-fn
  "Given a js global resolving to a function (e.g js/alert, js/console.log required-js-ns/js-fn), ensures it
  is called under the correct `this` context."
  [sym]
  (let [fields (str/split (name sym) #"\.")]
    `(.bind ~sym ~(symbol (namespace sym)
                    (if (seq (rest fields))
                      (str/join (interpose '. (butlast fields)))
                      "globalThis")))))

(defn resolve-static-field [sym]
  (when-some [ns (some-> (namespace sym) symbol)]
    (when-some [cls (resolve ns)]
      (when (class? cls)
        (clojure.lang.Reflector/getField cls (name sym) true)))))

(defn get-children-e [ts e] (-> ts :ave ::parent (get e)))
(defn get-child-e [ts e] (first (get-children-e ts e)))

(defn ?add-source-map [{{::keys [->id]} :o :as ts} pe form]
  (let [mt (meta form)]
    (cond-> ts (:line mt) (ts/add {:db/id (->id), ::source-map-of pe, ::line (:line mt), ::column (:column mt)}))))

(defn untwin [s]
  (if (= "cljs.core" (namespace s))
    (let [clj (symbol "clojure.core" (name s))]
      (if (resolve clj) clj s))
    s))

(tests
  (untwin 'cljs.core/prn) := 'clojure.core/prn
  (untwin 'a/b) := 'a/b
  (untwin 'a) := 'a
  (untwin 'cljs.core/not-in-clj) := 'cljs.core/not-in-clj)

(defn node? [mt] (::deps mt))
(defn resolve-node [sym env]
  (case (->env-type env)
    :clj (when-some [^clojure.lang.Var vr (resolve env sym)]
           (when (-> vr meta node?)
             (symbol (-> vr .ns str) (-> vr .sym str))))
    :cljs (when-some [vr (cljs-ana/find-var @!a sym (get-ns env)) #_(resolve-cljs env sym)]
            (when (-> vr ::cljs-ana/meta node?)
              (symbol (-> vr :name str))))))

(defn analyze-clj-symbol [sym ns$]
  (if (resolve-static-field sym)
    {::type ::static, ::sym sym}
    (when-some [v (ns-resolve (find-ns ns$) sym)]
      (if (var? v) {::type ::var, ::sym (symbol v)} {::type ::static, ::sym sym}))))

(def implicit-cljs-nses '#{goog goog.object goog.string goog.array Math String})

(defn analyze-cljs-symbol [sym env]
  (if-some [v (cljs-ana/find-var @!a sym (get-ns env))]
    {::type ::var, ::sym (untwin (::cljs-ana/name v))}
    {::type ::static, ::sym sym}))

(defn resolve-symbol [sym env]
  (if-some [local (-> env :locals (get sym))]
    (if-some [ref (::electric-let local)]
      {::lang nil, ::type ::let-ref, ::sym sym, ::ref ref}
      {::lang nil, ::type ::local, ::sym sym})
    (if-some [nd (resolve-node sym env)]
      {::lang nil, ::type ::node, ::node nd}
      (case (get (::peers env) (::current env))
        :clj (let [v (analyze-clj-symbol sym (get-ns env))] (case v nil (cannot-resolve! env sym) #_else (assoc v ::lang :clj)))
        :cljs (assoc (analyze-cljs-symbol sym env)
                ::lang :cljs)
        #_unsited (let [langs (set (vals (::peers env)))
                        vs (->> langs (into #{} (map #(case %
                                                        :clj (analyze-clj-symbol sym (get-ns env))
                                                        :cljs (analyze-cljs-symbol sym env)))))]
                    (cond (contains? vs nil) (cannot-resolve! env sym)
                          (> (count vs) 1) (ambiguous-resolve! env sym vs)
                          :else (assoc (first vs) ::lang nil)))))))

(defn ->let-val-e [ts e] (first (get-children-e ts e)))
(defn ->let-body-e [ts e] (second (get-children-e ts e)))

(defn get-ret-e [ts e]
  (let [nd (get (:eav ts) e)]
    (case (::type nd)
      ::let (recur ts (->let-body-e ts e))
      ::site (recur ts (get-child-e ts e))
      #_else e)))

(defn find-sitable-parent [ts e]
  (when-some [pe (::parent (get (:eav ts) e))]
    (case (::type (get (:eav ts) pe))
      ::site (recur ts pe)
      #_else pe)))

(defn get-site [ts e]
  (loop [e e]
    (when-some [nd (get (:eav ts) e)]
      (case (::type nd)
        ::let-ref (recur (->> nd ::ref (->let-val-e ts) (get-ret-e ts)))
        ::site (::site nd)
        #_else (recur (::parent nd))))))

(defn get-lookup-key [sym env]
  (if (symbol? sym)
    (let [{::keys [type sym]} (resolve-symbol sym env)]
      (case type
        (::static) (throw (ex-info (str "`" sym "` did not resolve as a var") {::form sym}))
        #_else (keyword sym)))
    sym))

(declare analyze)

(defn ->class-method-call [clazz method method-args pe env form {{::keys [->id]} :o :as ts}]
  (if (seq method-args)
    (let [e (->id), ce (->id)]
      (reduce (fn [ts form] (analyze form e env ts))
        (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap})
          (ts/add {:db/id ce, ::parent e, ::type ::pure})
          (ts/add {:db/id (->id), ::parent ce, ::type ::literal,
                   ::v (let [margs (repeatedly (count method-args) gensym), meth (symbol (str clazz) (str method))]
                         `(fn [~@margs] (~meth ~@margs)))}))
        method-args))
    (let [e (->id)]                     ; (. java.time.Instant now)
      (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure})
        (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form})))))

(defn ->obj-method-call [o method method-args pe env {{::keys [->id]} :o :as ts}]
  (let [e (->id), ce (->id)]
    (reduce (fn [ts form] (analyze form e env ts))
      (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap})
        (ts/add {:db/id ce, ::parent e, ::type ::pure})
        (ts/add {:db/id (->id), ::parent ce, ::type ::literal,
                 ::v (let [oo (gensym "o"), margs (repeatedly (count method-args) gensym)]
                       `(fn [~oo ~@margs] (. ~oo ~method ~@margs)))}))
      (cons o method-args))))

(defn def-sym-in-cljs-compiler! [sym ns]
  (swap! @(requiring-resolve 'cljs.env/*compiler*)
    assoc-in [:cljs.analyzer/namespaces ns :defs sym] {:name sym}))

(defn store [env form]
  (if (::last env)
    (update env ::last #(conj (pop %) form))
    (assoc env ::last (conj (clojure.lang.PersistentQueue/EMPTY) nil form))))

(defn analyze [form pe env {{::keys [->id]} :o :as ts}]
  (let [env (store env form)]
    (cond
      (and (seq? form) (seq form))
      (case (first form)
        (let*) (let [[_ bs bform] form]
                 (loopr [ts ts, pe pe, env env]
                     [[s v] (eduction (partition-all 2) bs)]
                     (let [e (->id)]
                       (recur (analyze v e env
                                (-> (ts/add ts {:db/id e, ::parent pe, ::type ::let, ::sym s})
                                  (?add-source-map e form))) e (update-in env [:locals s] assoc ::electric-let e)))
                     (analyze bform pe env ts)))
        (case) (let [[_ test & brs] form
                     [default brs2] (if (odd? (count brs)) [(last brs) (butlast brs)] [:TODO brs])]
                 (loopr [bs [], mp {}]
                     [[v br] (partition 2 brs2)]
                     (let [b (gensym "case-val")]
                       (recur (conj bs b `(::ctor ~br))
                         (reduce (fn [ac nx] (assoc ac (list 'quote nx) b)) mp (if (seq? v) v [v]))))
                     (recur (?meta form `(let* ~bs (::call (~mp ~test (::ctor ~default))))) pe env ts)))
        (quote) (let [e (->id)]
                  (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure})
                    (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form})))
        (fn*) (let [e (->id), ce (->id)
                    [form refs] (closure env form)
                    ts2 (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap})
                          (ts/add {:db/id ce, ::parent e, ::type ::pure})
                          (ts/add {:db/id (->id), ::parent ce, ::type ::literal, ::v form})
                          (?add-source-map e form))]
                (reduce (fn [ts nx] (analyze nx e env ts)) ts2 refs))
        (new) (let [[_ f & args] form, e (->id), ce (->id), cce (->id)]
                (reduce (fn [ts arg] (analyze arg e env ts))
                  (-> ts
                    (ts/add {:db/id e,   ::parent pe, ::type ::ap})
                    (ts/add {:db/id ce,  ::parent e,  ::type ::pure})
                    (ts/add {:db/id cce, ::parent ce, ::type ::literal,
                             ::v (let [gs (repeatedly (count args) gensym)]
                                   `(fn [~@gs] (new ~f ~@gs)))}))
                  args))
        ;; (. java.time.Instant now)
        ;; (. java.time.Instant ofEpochMilli 1)
        ;; (. java.time.Instant (ofEpochMilli 1))
        ;; (. java.time.Instant EPOCH)
        ;; (. java.time.Instant -EPOCH)
        ;; (. i1 isAfter i2)
        ;; (. i1 (isAfter i2))
        ;; (. pt x)
        ;; (. pt -x)
        (.) (cond
              (implicit-cljs-nses (second form)) ; (Math/abs -1) expanded to (. Math abs -1)
              (let [[_ clazz method & method-args] form] ; cljs fails on dot form, so we compile as class call
                (->class-method-call clazz method method-args pe env form ts))

              (and (symbol? (second form)) (class? (resolve env (second form))))
              (if (seq? (nth form 2))   ; (. java.time.Instant (ofEpochMilli 1))
                (let [[_ clazz [method & method-args]] form]
                  (->class-method-call clazz method method-args pe env form ts))
                (let [[_ clazz x & xs] form]
                  (->class-method-call clazz x xs pe env form ts)))

              (seq? (nth form 2))       ; (. i1 (isAfter i2))
              (let [[_ o [method & method-args]] form]
                (->obj-method-call o method method-args pe env ts))

              :else
              (let [[_ o x & xs] form]
                (if (seq xs)            ; (. i1 isAfter i2)
                  (->obj-method-call o x xs pe env ts)
                  (let [e (->id), ce (->id)] ; (. pt x)
                    (recur o e env
                      (-> ts
                        (ts/add {:db/id e, ::parent pe, ::type ::ap})
                        (ts/add {:db/id ce, ::parent e, ::type ::pure})
                        (ts/add {:db/id (->id) , ::parent ce, ::type ::literal, ::v `(fn [oo#] (. oo# ~x))})))))))
        (binding clojure.core/binding) (let [[_ bs bform] form, gs (repeatedly (/ (count bs) 2) gensym)]
                                         (recur (if (seq bs)
                                                  `(let* [~@(interleave gs (take-nth 2 (next bs)))]
                                                     (::call ((::static-vars r/bind) (::ctor ~bform)
                                                              ~@(interleave
                                                                  (mapv #(get-lookup-key % env) (take-nth 2 bs))
                                                                  (mapv #(list ::pure %) gs)))))
                                                  bform)
                                           pe env ts))
        (def) (let [[_ sym v] form]
                (case (->env-type env)
                  :clj (recur `((fn* ([x#] (def ~sym x#))) ~v) pe env ts)
                  :cljs (do (def-sym-in-cljs-compiler! sym (get-ns env))
                            (recur `(set! ~sym ~v) pe env ts))))
        (set!) (let [[_ target v] form] (recur `((fn* ([v#] (set! ~target v#))) ~v) pe env ts))
        (::ctor) (let [e (->id), ce (->id)]
                   (recur (list ::site nil (second form))
                     ce env (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure})
                              (ts/add {:db/id ce, ::parent e, ::type ::ctor})
                              (?add-source-map e form))))
        (::call) (let [e (->id)] (recur (second form) e env (-> (ts/add ts {:db/id e, ::parent pe, ::type ::call})
                                                              (?add-source-map e form))))
        (::pure) (let [e (->id)] (recur (second form) e env (-> (ts/add ts {:db/id e, ::parent pe, ::type ::pure})
                                                              (?add-source-map e form))))
        (::join) (let [e (->id)] (recur (second form) e env (-> (ts/add ts {:db/id e, ::parent pe, ::type ::join})
                                                              (?add-source-map e form))))
        (::site) (let [[_ site bform] form, e (->id)]
                   (recur bform e (assoc env ::current site)
                     (-> (ts/add ts {:db/id e, ::parent pe, ::type ::site, ::site site})
                       (?add-source-map e form))))
        (::lookup) (let [[_ sym] form] (ts/add ts {:db/id (->id), ::parent pe, ::type ::lookup, ::sym sym}))
        (::static-vars) (recur (second form) pe (assoc env ::static-vars true) ts)
        #_else (let [e (->id)]
                 (reduce (fn [ts nx] (analyze nx e env ts)) (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap})
                                                              (?add-source-map e form)) form)))

      (instance? cljs.tagged_literals.JSValue form)
      (let [o (.-val ^cljs.tagged_literals.JSValue form)]
        (if (map? o)
          (recur (?meta form (cons `(::static-vars cljs.core/js-obj) (into [] (mapcat (fn [[k v]] [(name k) v])) o)))
            pe env ts)
          (recur (?meta form (cons `(::static-vars cljs.core/array) o)) pe env ts)))

      (vector? form) (recur (?meta form (cons `(::static-vars vector) form)) pe env ts)
      (map? form) (recur (?meta form (cons `(::static-vars hash-map) (eduction cat form))) pe env ts)
      (set? form) (recur (?meta form (cons `(::static-vars hash-set) form)) pe env ts)

      (symbol? form)
      (let [e (->id), ret (resolve-symbol form env)]
        (-> (case (::type ret)
              (::let-ref) (ts/add ts {:db/id e, ::parent pe, ::type ::let-ref, ::ref (::ref ret), ::sym form})
              (::local) (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure})
                          (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form}))
              (::static ::var) (if (::static-vars env)
                                 (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure})
                                   (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form}))
                                 (ts/add ts (cond-> {:db/id e, ::parent pe, ::type ::var
                                                     ::var form, ::qualified-var (::sym ret)}
                                              (::lang ret) (assoc ::resolved-in (::lang ret)))))
              (::node) (ts/add ts {:db/id e, ::parent pe, ::type ::node, ::node (::node ret)})
              #_else (throw (ex-info (str "unknown symbol type " (::type ret)) (or ret {}))))
          (?add-source-map e form)))

      :else
      (let [e (->id)]
        (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure})
          (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form})
          (?add-source-map e form))))))

(defn ->->id [] (let [!i (long-array [-1])] (fn [] (aset !i 0 (unchecked-inc (aget !i 0))))))

(defn find-ctor-e [ts e]
  (let [pe (::parent (get (:eav ts) e))]
    (if (or (nil? pe) (= ::ctor (::type (get (:eav ts) pe)))) pe (recur ts pe))))

(defn get-node-idx [ts ctor-e ref-e]
  (->> (ts/find ts ::ctor-node ctor-e, ::ctor-ref ref-e) first (ts/->node ts) ::node-idx))

(defn emit [ts e ctor-e env nm]
  ((fn rec [e]
     (let [nd (get (:eav ts) e)]
       (case (::type nd)
         ::literal (::v nd)
         ::ap (list* `r/ap (mapv rec (get-children-e ts e)))
         ::var (let [in (::resolved-in nd)]
                 (list* `r/lookup 'frame (keyword (::qualified-var nd))
                   (when (or (nil? in) (= in (->env-type env))) [(list `r/pure (::qualified-var nd))])))
         ::node (list `r/lookup 'frame (keyword (::node nd)) (list `r/pure (list `r/make-ctor 'frame (keyword (::node nd)) 0)))
         ::join (list `r/join (rec (get-child-e ts e)))
         ::pure (list `r/pure (rec (get-child-e ts e)))
         ::comp (doall (map rec (get-children-e ts e)))
         ::site (recur (get-child-e ts e))
         ::ctor (let [ctor (list `r/make-ctor 'frame nm (::ctor-idx nd))
                      frees-e (-> ts :ave ::ctor-free (get e))]
                  (if (seq frees-e)
                    (list* `doto ctor
                      (mapv (fn [e]
                              (let [nd (ts/->node ts e)]
                                (list `r/define-free (::free-idx nd)
                                  (case (::closed-over nd)
                                    ::node (list `r/node 'frame (get-node-idx ts (find-ctor-e ts (::ctor-free nd)) (::closed-ref nd)))
                                    ::free (list `r/free 'frame (->> (ts/find ts ::ctor-free (find-ctor-e ts (::ctor-free nd))
                                                                       ::closed-ref (::closed-ref nd))
                                                                  first (ts/->node ts) ::free-idx))))))
                        frees-e))
                    ctor))
         ::call (list `r/join (list `r/call 'frame (::call-idx (ts/->node ts e))))
         ::lookup (list `r/lookup 'frame (::sym nd))
         ::let (recur (get-ret-e ts (->let-body-e ts e)))
         ::let-ref
         (if-some [node-e (first (ts/find ts ::ctor-node ctor-e, ::ctor-ref (::ref nd)))]
           (list `r/node 'frame (::node-idx (get (:eav ts) node-e)))
           (if-some [free-e (first (ts/find ts ::ctor-free ctor-e, ::closed-ref (::ref nd)))]
             (list `r/free 'frame (::free-idx (ts/->node ts free-e)))
             (recur (get-ret-e ts (->let-val-e ts (::ref nd))))))
         #_else (throw (ex-info (str "cannot emit on " (pr-str (::type nd))) (or nd {}))))))
   e))

(defn emit-node-init [ts ctor-e node-e env nm]
  (let [nd (get (:eav ts) node-e)]
    (list `r/define-node 'frame (::node-idx nd)
      (emit ts (get-ret-e ts (->let-val-e ts (::ctor-ref nd))) ctor-e env nm))))

(defn emit-call-init [ts ctor-e e env nm]
  (list `r/define-call 'frame (::call-idx (ts/->node ts e))
    (emit ts (get-ret-e ts (get-child-e ts e)) ctor-e env nm)))

(defn get-ordered-ctors-e [ts]
  (into [] (map (comp first second)) (->> ts :ave ::ctor-idx (sort-by first))))

(defn get-ordered-calls-e [ts ctor-e]
  (->> (ts/find ts ::ctor-call ctor-e) (sort-by #(::call-idx (ts/->node ts %)))))

(defn get-ordered-nodes-e [ts ctor-e]
  (->> (ts/find ts ::ctor-node ctor-e) (sort-by #(::node-idx (ts/->node ts %)))))

(defn compute-effect-order [ts e]
  (let [->order (->->id), ord (fn [ts e] (ts/upd ts e ::fx-order #(or % (->order)))), seen (volatile! #{})]
    ((fn rec [ts e]
       (let [nd (ts/->node ts e)]
         (if (@seen e)
           ts
           (do (vswap! seen conj e)
               (case (::type nd)
                 (::literal ::var ::lookup ::node) (ord ts e)
                 (::ap ::comp) (ord (reduce rec ts (get-children-e ts e)) e)
                 (::site ::join ::pure ::call ::ctor) (ord (rec ts (get-child-e ts e)) e)
                 (::let) (recur ts (->let-body-e ts e))
                 (::let-ref) (ord (rec ts (->let-val-e ts (::ref nd))) (::ref nd))
                 #_else (throw (ex-info (str "cannot compure-effect-order on " (pr-str (::type nd))) (or nd {})))
                 )))))
     ts e)))

(defn emit-ctor [ts ctor-e env nm]
  (let [ret-e (get-ret-e ts (get-child-e ts ctor-e))
        nodes-e (get-ordered-nodes-e ts ctor-e)
        calls-e (get-ordered-calls-e ts ctor-e)]
    `(r/cdef ~(count (ts/find ts ::ctor-free ctor-e))
       ~(mapv #(get-site ts (->> (ts/->node ts %) ::ctor-ref (->let-val-e ts) (get-ret-e ts)))
          nodes-e)
       ~(mapv #(get-site ts %) calls-e)
       ~(get-site ts ret-e)
       (fn [~'frame]
         ~@(let [node-inits (->> nodes-e (mapv (fn [e] [(->> e (ts/->node ts) ::ctor-ref (ts/->node ts) ::fx-order)
                                                        (emit-node-init ts ctor-e e env nm)])))
                 call-inits (->> calls-e (mapv (fn [e] [(->> e (ts/->node ts) ::fx-order)
                                                        (emit-call-init ts ctor-e e env nm)])))]
             ;; with xforms would be
             ;; (into [] (comp cat (x/sort-by first) (map second)) [node-inits call-inits])
             (->> (concat node-inits call-inits) (sort-by first) (eduction (map second))))
         ~(emit ts ret-e ctor-e env nm)))))

(defn emit-deps [ts e]
  (let [seen (volatile! #{})
        mark (fn mark [ts e]
               (if (@seen e)
                 ts
                 (let [nd (ts/->node ts e)]
                   (vswap! seen conj e)
                   (case (::type nd)
                     (::literal ::var ::lookup) ts
                     (::ap ::comp) (reduce mark ts (get-children-e ts e))
                     (::site ::join ::pure ::call ::ctor) (recur ts (get-child-e ts e))
                     (::let) (recur ts (->let-body-e ts e))
                     (::let-ref) (recur ts (get-ret-e ts (->let-val-e ts (::ref nd))))
                     (::node) (ts/asc ts e ::node-used true)
                     #_else (throw (ex-info (str "cannot emit-deps/mark on " (pr-str (::type nd))) (or nd {})))))))
        es (ts/find (mark ts e) ::node-used true)]
    (into (sorted-set) (map #(::node (ts/->node ts %))) es)))

(defn get-deps [sym] (-> sym resolve meta ::deps))

(defn analyze-electric [env {{::keys [->id]} :o :as ts}]
  (let [change-parent (fn change-parent [ts e pe] (ts/asc ts e ::parent pe))
        orphan (fn orphan [ts e] (change-parent ts e nil))
        collapse-ap-with-only-pures (fn collapse-ap-with-only-pures [ts] ; (r/ap (r/pure .)+ ) => (r/pure (::comp . . .))
                                      (reduce (fn [ts ap-e]
                                                (let [[f-e & args-e :as children-e] (get-children-e ts ap-e)]
                                                  (if (every? #(= ::pure (::type (ts/->node ts %))) children-e)
                                                    (reduce (fn [ts e]
                                                              (-> ts (change-parent (get-child-e ts e) f-e)
                                                                (orphan e)))
                                                      ;; reuse nodes, otherwise node ordering messes up
                                                      (-> ts (ts/asc ap-e ::type ::pure) (ts/asc f-e ::type ::comp))
                                                      args-e)
                                                    ts)))
                                        ts (reverse (ts/find ts ::type ::ap))))
        ->ctor-idx (->->id)
        seen (volatile! #{})
        mark-used-ctors (fn mark-used-ctors [ts e]
                          (if (@seen e)
                            ts
                            (let [nd (get (:eav ts) e)]
                              (vswap! seen conj e)
                              (case (::type nd)
                                (::literal ::var ::lookup ::node) ts
                                (::ap ::comp) (reduce mark-used-ctors ts (get-children-e ts e))
                                (::site ::join ::pure ::call) (recur ts (get-child-e ts e))
                                (::ctor) (if (::ctor-idx nd)
                                           ts
                                           (recur (ts/asc ts e ::ctor-idx (->ctor-idx)) (get-child-e ts e)))
                                (::let) (recur ts (->let-body-e ts e))
                                (::let-ref) (recur ts (get-ret-e ts (->let-val-e ts (::ref nd))))
                                #_else (throw (ex-info (str "cannot mark-used-ctors on " (pr-str (::type nd))) (or nd {})))))))
        ts (-> ts collapse-ap-with-only-pures
             (compute-effect-order 0)
             (mark-used-ctors 0))
        ctors-e (get-ordered-ctors-e ts)
        ensure-node (fn ensure-node [ts ref-e]
                      (let [ctor-e (find-ctor-e ts ref-e)]
                        (cond-> ts (empty? (ts/find ts ::ctor-ref ref-e))
                                (ts/add {:db/id (->id) ::ctor-node ctor-e, ::ctor-ref ref-e}))))
        ensure-free-node (fn ensure-free-node [ts ref-e ctor-e]
                           (cond-> ts (empty? (ts/find ts ::ctor-free ctor-e, ::closed-ref ref-e))
                                   (ts/add {:db/id (->id) ::ctor-free ctor-e, ::closed-ref ref-e, ::closed-over ::node})))
        ensure-free-free (fn ensure-free-free [ts ref-e ctor-e]
                           (cond-> ts (empty? (ts/find ts ::ctor-free ctor-e, ::closed-ref ref-e))
                                   (ts/add {:db/id (->id) ::ctor-free ctor-e, ::closed-ref ref-e, ::closed-over ::free})))
        ensure-free-frees (fn ensure-free-frees [ts ref-e ctors-e]
                            (reduce (fn [ts ctor-e] (ensure-free-free ts ref-e ctor-e)) ts ctors-e))
        order-nodes (fn order-nodes [ts]
                      (reduce (fn [ts nodes-e]
                                (let [->idx (->->id)]
                                  (reduce (fn [ts e] (ts/asc ts e ::node-idx (->idx)))
                                    ts (sort-by #(->> % (ts/->node ts) ::ctor-ref (ts/->node ts) ::fx-order)
                                         nodes-e))))
                        ts (-> ts :ave ::ctor-node vals)))
        order-frees (fn order-frees [ts]
                      (reduce (fn [ts frees-e]
                                (let [->idx (->->id)]
                                  (reduce (fn [ts e] (ts/asc ts e ::free-idx (->idx)))
                                    ts (sort-by #(::fx-order (ts/->node ts %)) frees-e) #_(sort-by-fx-order ts frees-e))))
                        ts (-> ts :ave ::ctor-free vals)))
        in-a-call? (fn in-a-call? [ts e]
                     (loop [e (::parent (ts/->node ts e))]
                       (when-let [nd (ts/->node ts e)]
                         (case (::type nd)
                           ::call true
                           ::ctor false
                           #_else (recur (::parent nd))))))
        handle-let-refs (fn handle-let-refs [ts e] ; nodes and frees (closed over)
                          (let [nd (ts/->node ts e)]
                            (case (::type nd)
                              (::literal ::var ::lookup ::node) ts
                              (::ap ::comp) (reduce handle-let-refs ts (get-children-e ts e))
                              (::site ::join ::pure ::ctor ::call) (recur ts (get-child-e ts e))
                              (::let) (recur ts (->let-body-e ts e))
                              (::let-ref)
                              (let [ref-nd (ts/->node ts (::ref nd))
                                    ctors-e (loop [ac '(), e (::parent (ts/->node ts e))]
                                              (if (= (::ref nd) e)
                                                ac
                                                (recur (cond-> ac (= ::ctor (::type (ts/->node ts e))) (conj e))
                                                  (::parent (ts/->node ts e)))))
                                    ts (ts/asc ts (::ref nd) ::walked-val true) ; only walk binding once
                                    ;; TODO is this necessary? If not we could inline more
                                    ;; ts (cond-> ts (in-a-call? ts e)
                                    ;;            (-> (ts/upd (::ref nd) ::in-call #(conj (or % #{}) e))
                                    ;;              (ensure-node (::ref nd))))
                                    ts (if (seq ctors-e) ; closed over
                                         (-> ts (ensure-node (::ref nd))
                                           (ensure-free-node (::ref nd) (first ctors-e))
                                           (ensure-free-frees (::ref nd) (rest ctors-e)))
                                         (cond-> (ts/upd ts (::ref nd) ::refcnt (fnil inc 0))
                                           (or (= 1 (::refcnt ref-nd))
                                             (not= (get-site ts (find-sitable-parent ts e))
                                               (get-site ts (->let-val-e ts (::ref nd)))))
                                           (ensure-node (::ref nd))))]
                                (cond-> ts
                                  (not (::walked-val ref-nd)) (recur (get-ret-e ts (->let-val-e ts (::ref nd))))))
                              #_else (throw (ex-info (str "cannot handle-let-refs on " (::type nd)) (or nd {}))))))
        ->call-idx (let [mp (zipmap ctors-e (repeatedly ->->id))]
                     (fn ->call-idx [ctor-e] ((get mp ctor-e))))
        seen (volatile! #{})
        mark-used-calls (fn mark-used-calls [ts ctor-e e]
                          (if (@seen e)
                            ts
                            (let [nd (ts/->node ts e)]
                              (vswap! seen conj e)
                              (case (::type nd)
                                (::literal ::var ::lookup ::node) ts
                                (::ap ::comp) (reduce #(mark-used-calls % ctor-e %2) ts (get-children-e ts e))
                                (::site ::join ::pure) (recur ts ctor-e (get-child-e ts e))
                                (::ctor) (recur ts e (get-child-e ts e))
                                (::call) (if (::call-idx nd)
                                           ts
                                           (-> (mark-used-calls ts ctor-e (get-child-e ts e))
                                             (ts/asc e ::call-idx (->call-idx ctor-e))
                                             (ts/asc e ::ctor-call ctor-e)))
                                (::let) (recur ts ctor-e (->let-body-e ts e))
                                (::let-ref) (let [nx-e (get-ret-e ts (->let-val-e ts (::ref nd)))]
                                              (recur ts (find-ctor-e ts nx-e) nx-e))
                                #_else (throw (ex-info (str "cannot mark-used-calls on " (::type nd)) (or nd {})))))))
        ts (-> ts (handle-let-refs 0) order-nodes order-frees
             (mark-used-calls 0 (get-ret-e ts (get-child-e ts 0))))]
    (when (::print-db env) (run! prn (->> ts :eav vals (sort-by :db/id))))
    ts))

(defn compile* [nm env ts]
  (let [ts (analyze-electric env ts)
        ret (->> (get-ordered-ctors-e ts) (mapv #(emit-ctor ts % env nm)))]
    (when (::print-source env) (fipp.edn/pprint ret))
    ret))

(defn compile [nm form env]
  (compile* nm env
    (analyze (expand-all env `(::ctor ~form))
      '_ env (ts/->ts {::->id (->->id)}))))
