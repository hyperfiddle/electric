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
    (try (macroexpand-1 o)   ; e.g. (Math/abs 1) will expand to (. Math abs 1)
         (catch ClassNotFoundException _ o)))) ; e.g. (goog.color/hslToHex ..) won't expand on clj

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

(defmacro $ [F & args]
  `(::call ((::static-vars r/dispatch) ~F ~@(map (fn [arg] `(::pure ~arg)) args))))

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
                  (recur (?meta o `(::call (r/bind-args (r/bind-self (::ctor (let [~@(interleave (take-nth 2 bs2)
                                                                                       (map (fn [i] `(::lookup ~i))
                                                                                         (range)))] ~@body)))
                                             ~@(map (fn [arg] `(::pure ~arg))
                                                 (take-nth 2 (next bs2))))))
                    env2))

        (recur) (recur (?meta o `(::call (r/bind-args (::lookup :recur) ~@(map (fn [arg] `(::pure ~arg)) (next o))))) env)

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
                   (recur (?meta o `(let [~(vec (take-nth 2 bs2)) (::cc-letfn ~bs2)] ~(-expand-all (cons 'do body) env2)))
                     env))

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
  (let [expanded (-expand-all o (assoc env ::electric true))]
    (when (::print-expansion env) (fipp.edn/pprint expanded))
    expanded))

;;;;;;;;;;;;;;;;
;;; COMPILER ;;;
;;;;;;;;;;;;;;;;

(defn fail!
  ([env msg] (fail! env msg {}))
  ([env msg data] (throw (ex-info (str "\n" (get-ns env) (when-some [d (::def env)] (str "/" d)) ":" (-> env ::meta :line) ":" (-> env ::meta :column) "\n" msg)
                           (merge {:in (::def env) :for (or (::current env) ::unsited)} data)))))

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
        edef?           (fn [ast] (and (= :var (:op ast)) (not (-> ast :env :def-var))))
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
        var-set?        (fn [ast] (and (= :set! (:op ast)) (= :var (:op (:target ast)))))
        rewrite-cljs-ast (fn walk [ast]
                           (cond
                             (var-set? ast) (update ast :val walk)
                             (edef? ast)    (do (record-edef! ast)
                                                (cond (dynamic? ast)    (qualify-sym-in-var-node env ast)
                                                      (namespaced? ast) (update ast :form safe-let-name)
                                                      :else             ast))
                             (lexical? ast) (do (record-lexical! ast) ast)
                             :else          (let [quald-ast (qualify-sym-in-var-node env ast)]
                                              (if-some [cs (:children quald-ast)]
                                                (reduce (fn [ast k]
                                                          (update ast k #(if (vector? %) (mapv walk %) (walk %))))
                                                  quald-ast cs)
                                                quald-ast))))
        form            (case (or (get (::peers env) (::current env)) (->env-type env))
                          :clj  (-> (ana/analyze-clj (update env :ns :name) form)
                                  (ana/walk-clj rewrite-ast)
                                  (ana/emit-clj))
                          :cljs (-> (binding [cljs.analyzer/*cljs-warning-handlers*
                                              [(fn [_warning-type _env _extra])]]
                                      (ana/analyze-cljs env form))
                                  (rewrite-cljs-ast)
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
(defn ?get-child-e [ts e] (first (get-children-e ts e)))
(defn get-child-e [ts e] (ca/is (first (get-children-e ts e)) some? (str "no child for " e) {:e e, :nd (ts/->node ts e)}))
(defn get-root-e [ts] (get-child-e ts '_))

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
           (when (-> vr meta node?) (symbol vr)))
    :cljs (when-some [vr (cljs-ana/find-var @!a sym (get-ns env))]
            ;; temporary hack
            ;; the commented out expression should work, seems the new cljs analyzer loses the metadata
            ;; so we check it on clj side, which is safe for a clj-server/cljs-client setup
            (when-some [vr (cljs-ana/safe-requiring-resolve (-> vr ::cljs-ana/name))]
              (when (-> vr meta node?) (symbol vr)))
            #_(when (-> vr ::cljs-ana/meta node?)
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
    (if-some [uid (::electric-let local)]
      {::lang nil, ::type ::localref, ::sym sym, ::ref uid}
      {::lang nil, ::type ::local, ::sym sym})
    (if (= sym (::def env))
      {::lang nil, ::type ::self, ::sym sym}
      (if-some [nd (resolve-node sym env)]
        {::lang nil, ::type ::node, ::node nd}
        (case (get (::peers env) (::current env))
          :clj (let [v (analyze-clj-symbol sym (get-ns env))] (case v nil (cannot-resolve! env sym) #_else (assoc v ::lang :clj)))
          :cljs (assoc (analyze-cljs-symbol sym env) ::lang :cljs)
          #_unsited (case (->env-type env)
                      :clj (assoc (or (analyze-clj-symbol sym (get-ns env)) {::type ::var, ::sym `r/cannot-resolve}) :lang :clj)
                      :cljs (assoc (analyze-cljs-symbol sym env) :lang :cljs)))))))


(defn ->bindlocal-body-e [ts e] (second (get-children-e ts e)))
(defn ->localv-e [ts mklocal-uid]
  (->> (ts/find ts ::type ::bindlocal, ::ref mklocal-uid) first (get-child-e ts)))


(defn get-ret-e [ts e]
  (let [nd (get (:eav ts) e)]
    (case (::type nd)
      (::bindlocal) (recur ts (->bindlocal-body-e ts e))
      (::site ::mklocal) (recur ts (get-child-e ts e))
      #_else e)))

(defn find-sitable-point-e [ts e]
  (loop [e e]
    (when-some [nd (ts/->node ts e)]
      (case (::type nd)
        (::literal ::ap ::join ::pure ::comp ::ctor ::call) e
        (::site) (when (some? (::site nd)) (recur (::parent nd)))
        (::var ::node ::lookup ::mklocal ::bindlocal ::localref) (some-> (::parent nd) recur)
        #_else (throw (ex-info (str "can't find-sitable-point-e for " (pr-str (::type nd))) (or nd {})))))))

(defn get-site [ts e]
  (loop [e (find-sitable-point-e ts e)]
    (when-some [nd (get (:eav ts) e)]
      (case (::type nd)
        ::site (::site nd)
        #_else (recur (::parent nd))))))

(defn get-lookup-key [sym env]
  (if (symbol? sym)
    (let [it (resolve-symbol sym env)]
      (case (::type it)
        (::var) (keyword (::sym it))
        (::node) (keyword (::node it))
        (::static) (throw (ex-info (str "`" sym "` did not resolve as a var") {::form sym}))
        #_else (keyword sym)))
    sym))

(declare analyze)

;; Due to an early bad assumption only `let` bound values are considered
;; for nodes (`r/define-node`). But in `(e/client (name (e/server :x)))`
;; `:x` needs to be a node too. For this reason we wrap function arguments
;; in an implicit `let`. This doesn't increase the generated code size
;; because `handle-let-refs` is smart enough to inline wherever possible.
(defn wrap-ap-arg [form]
  (if (or (symbol? form) (keyword? form) (number? form))
    form
    (let [ap-arg (gensym "ap-arg")] `(let* [~ap-arg ~form] ~ap-arg))) #_form)

(defn ap-literal [f args pe e env {{::keys [->id ->uid]} :o :as ts}]
  (let [ce (->id)]
    (reduce (fn [ts form] (analyze form e env ts))
      (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap, ::uid (->uid)})
        (ts/add {:db/id ce, ::parent e, ::type ::pure})
        (ts/add {:db/id (->id), ::parent ce, ::type ::literal, ::v f}))
      (mapv wrap-ap-arg args))))

(defn ->class-method-call [clazz method method-args pe env form {{::keys [->id]} :o :as ts}]
  (if (seq method-args)
    (let [f (let [margs (repeatedly (count method-args) gensym), meth (symbol (str clazz) (str method))]
              `(fn [~@margs] (~meth ~@margs)))]
      (ap-literal f method-args pe (->id) env ts))
    (let [e (->id)]                     ; (. java.time.Instant now)
      (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure})
        (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form})))))

(defn meta-of-key [mp k] (-> mp keys set (get k) meta))
(defn gensym-with-local-meta [env k]
  (let [g (gensym (if (instance? clojure.lang.Named k) (name k) "o")), mt (meta-of-key (:locals env) k)]
    (with-meta g (merge mt (meta k)))))

(defn ->obj-method-call [o method method-args pe env {{::keys [->id]} :o :as ts}]
  (let [f (let [[oo & margs] (mapv #(gensym-with-local-meta env %) (cons o method-args))]
            `(fn [~oo ~@margs] (. ~oo ~method ~@margs)))]
    (ap-literal f (cons o method-args) pe (->id) env ts)))

(defn def-sym-in-cljs-compiler! [sym ns]
  (swap! @(requiring-resolve 'cljs.env/*compiler*)
    assoc-in [:cljs.analyzer/namespaces ns :defs sym] {:name sym}))

(defn e->uid [ts e] (ca/is (::uid (ts/->node ts e)) some? "node without uid" {:e e, :nd (ts/->node ts e)}))
(defn uid->e [ts uid] (first (ca/check #(= 1 (count %)) (ts/find ts ::uid uid))))
(defn reparent-children [ts from-e to-e]
  (reduce (fn [ts e] (ts/asc ts e ::parent to-e)) ts (ts/find ts ::parent from-e)))

(defn ?update-meta [env form] (cond-> env (meta form) (assoc ::meta (meta form))))

(defn analyze [form pe env {{::keys [->id ->uid]} :o :as ts}]
  (let [env (?update-meta env form)]
    (cond
      (and (seq? form) (seq form))
      (case (first form)
        (let*) (let [[_ bs bform] form]
                 (recur (?meta form
                          (reduce (fn [ac [k v]]
                                    `(::mklocal k# (::bindlocal k# ~v (::mklocal ~k (::bindlocal ~k k# ~ac)))))
                            bform (->> bs (partition 2) reverse)))
                   pe env ts))
        (::mklocal) (let [[_ k bform] form, e (->id), uid (->uid)
                          ts (-> ts (ts/add {:db/id e, ::parent pe, ::type ::mklocal, ::k k, ::uid uid})
                               (?add-source-map e form))]
                      (recur bform e (update-in env [:locals k] assoc ::electric-let uid) ts))
        (::bindlocal) (let [[_ k v bform] form, e (->id)
                            ts (ts/add ts {:db/id e, ::parent pe, ::type ::bindlocal ::k k
                                           ::ref (-> env :locals (get k) ::electric-let)})
                            ts (analyze v e env ts)]
                        (recur bform e env ts))
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
        (fn*) (let [e (->id), [form refs] (closure env form)]
                (ap-literal form refs pe e env (?add-source-map ts e form)))
        (::cc-letfn) (let [[_ bs] form, [form refs] (closure env `(letfn* ~bs ~(vec (take-nth 2 bs)))), e (->id)]
                       (ap-literal form refs pe e env (?add-source-map ts e form)))
        (new) (let [[_ f & args] form, current (get (::peers env) (::current env))]
                (if (or (nil? current) (= (->env-type env) current))
                  (let [f (let [gs (repeatedly (count args) gensym)] `(fn [~@gs] (new ~f ~@gs)))]
                    (ap-literal f args pe (->id) env ts))
                  (recur `[~@args] pe env ts)))
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
                  (ap-literal `(fn [oo#] (. oo# ~x)) [o] pe (->id) env ts)))) ; (. pt x)
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
                            (ap-literal `(fn [v#] (set! ~sym v#)) [v] pe (->id) env ts))))
        (set!) (let [[_ target v] form] (recur `((fn* ([v#] (set! ~target v#))) ~v) pe env ts))
        (::ctor) (let [e (->id), ce (->id)]
                   (recur (list ::site nil (second form))
                     ce env (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure})
                              (ts/add {:db/id ce, ::parent e, ::type ::ctor, ::uid (->uid)})
                              (?add-source-map e form))))
        (::call) (let [e (->id)] (recur (second form) e env
                                   (-> (ts/add ts {:db/id e, ::parent pe, ::type ::call, ::uid (->uid)})
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
        #_else (let [e (->id), uid (->uid)]
                 (reduce (fn [ts nx] (analyze (wrap-ap-arg nx) e env ts))
                   (-> (ts/add ts {:db/id e, ::parent pe, ::type ::ap, ::uid uid})
                     (?add-source-map uid form)) form)))

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
              (::localref) (ts/add ts {:db/id e, ::parent pe, ::type ::localref, ::ref (::ref ret)
                                       ::sym form, ::uid (->uid)})
              (::local) (-> ts (ts/add {:db/id e, ::parent pe, ::type ::pure})
                          (ts/add {:db/id (->id), ::parent e, ::type ::literal, ::v form}))
              (::self) (let [ce (->id)]
                         (-> ts
                           (ts/add {:db/id e, ::parent pe, ::type ::lookup, ::sym (keyword (ns-qualify form))})
                           (ts/add {:db/id ce, ::parent e, ::type ::pure})
                           (ts/add {:db/id (->id), ::parent ce, ::type ::literal, ::v (list form)})))
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

(defn- ts->reducible* [ts f init]
  (loop [ac init, es (cons (get-root-e ts) (set/difference (-> ts :eav keys set) (->> ts :ave ::parent vals (reduce into)))), seen #{}]
    (if (or (reduced? ac) (empty? es))
      (unreduced ac)
      (let [[e & es] es]
        (if (seen e)
          (recur ac es seen)
          (recur (f ac (ts/->node ts e)) (concat (get-children-e ts e) es) (conj seen e)))))))

(defn ts->reducible [ts]
  (reify clojure.lang.IReduce
    (reduce [_ f init] (ts->reducible* ts f init))
    (reduce [_ f] (ts->reducible* ts f (f)))))

(defn get-node-idx [ts ctor-uid uid]
  (->> (ts/find ts ::ctor-node ctor-uid, ::ctor-ref uid) first (ts/->node ts) ::node-idx))

(defn ->thunk [xs] `(fn* [] (~@xs)))

(defn emit [ts e ctor-e env nm]
  ((fn rec [e]
     (let [nd (get (:eav ts) e)]
       (case (::type nd)
         ::literal (::v nd)
         ::ap (list* `r/ap (mapv rec (get-children-e ts e)))
         ::var (let [in (::resolved-in nd)]
                 (list* `r/lookup 'frame (keyword (::qualified-var nd))
                   (when (or (nil? in) (= in (->env-type env))) [(list `r/pure (::qualified-var nd))])))
         ::node (list `r/lookup 'frame (keyword (::node nd)) (list `r/pure (list `r/resolve 'frame (keyword (::node nd)))))
         ::join (list `r/join (rec (get-child-e ts e)))
         ::pure (list `r/pure (rec (get-child-e ts e)))
         ::comp ((or (::comp-fn nd) ->thunk) (eduction (map rec) (get-children-e ts e))) #_(list 'fn* '[] (doall (map rec (get-children-e ts e))))
         ::site (recur (get-child-e ts e))
         ::ctor (list* `r/ctor nm (::ctor-idx nd)
                  (mapv (fn [e]
                          (let [nd (ts/->node ts e)]
                            (case (::closed-over nd)
                              ::node (list `r/node 'frame
                                       (get-node-idx ts
                                         (e->uid ts (find-ctor-e ts (uid->e ts (::ctor-free nd))))
                                         (::closed-ref nd)))
                              ::free (list `r/free 'frame
                                       (->> (ts/find ts
                                              ::ctor-free (e->uid ts
                                                            (find-ctor-e ts (uid->e ts (::ctor-free nd))))
                                              ::closed-ref (::closed-ref nd))
                                         first (ts/->node ts) ::free-idx)))))
                    (ts/find ts ::ctor-free (e->uid ts e))))
         ::call (list `r/join (list `r/call 'frame (::call-idx (ts/->node ts e))))
         ::lookup (list* `r/lookup 'frame (::sym nd) (when-some [c (?get-child-e ts e)] (list (rec c))))
         ::mklocal (recur (get-ret-e ts (get-child-e ts e)))
         ::bindlocal (recur (get-ret-e ts (->bindlocal-body-e ts e)))
         ::localref
         (if-some [node-e (first (ts/find ts ::ctor-node (e->uid ts ctor-e), ::ctor-ref (::ref nd)))]
           (list `r/node 'frame (::node-idx (ts/->node ts node-e)))
           (if-some [free-e (first (ts/find ts ::ctor-free (e->uid ts ctor-e), ::closed-ref (::ref nd)))]
             (list `r/free 'frame (::free-idx (ts/->node ts free-e)))
             (throw (ex-info "localref must be a node or free" nd))))
         #_else (throw (ex-info (str "cannot emit on " (pr-str (::type nd))) (or nd {}))))))
   e))

(defn emit-node-init [ts ctor-e node-e env nm]
  (let [nd (get (:eav ts) node-e)]
    (list `r/define-node 'frame (::node-idx nd)
      (emit ts (->> (::ctor-ref nd) (->localv-e ts) (get-ret-e ts)) ctor-e env nm))))

(defn emit-call-init [ts ctor-e e env nm]
  (list `r/define-call 'frame (::call-idx (ts/->node ts e))
    (emit ts (get-ret-e ts (get-child-e ts e)) ctor-e env nm)))

(defn get-ordered-ctors-e [ts] (into [] (map (comp first second)) (->> ts :ave ::ctor-idx (sort-by first))))

(defn get-ordered-calls-e [ts ctor-uid]
  (->> (ts/find ts ::ctor-call ctor-uid) (sort-by #(::call-idx (ts/->node ts %)))))

(defn get-ordered-nodes-e [ts ctor-uid]
  (->> (ts/find ts ::ctor-node ctor-uid) (sort-by #(::node-idx (ts/->node ts %)))))

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
                 (::site ::join ::pure ::call ::ctor ::mklocal) (ord (rec ts (get-child-e ts e)) e)
                 (::bindlocal) (recur ts (->bindlocal-body-e ts e))
                 (::localref) (ord (rec ts (->localv-e ts (::ref nd))) (uid->e ts (::ref nd)))
                 #_else (throw (ex-info (str "cannot compure-effect-order on " (pr-str (::type nd))) (or nd {})))
                 )))))
     ts e)))

(defn emit-ctor [ts ctor-e env nm]
  (let [ret-e (get-ret-e ts (get-child-e ts ctor-e))
        ctor-uid (::uid (ts/->node ts ctor-e))
        nodes-e (get-ordered-nodes-e ts ctor-uid)
        calls-e (get-ordered-calls-e ts ctor-uid)]
    `(r/cdef ~(count (ts/find ts ::ctor-free ctor-uid))
       ~(mapv #(get-site ts (->> (ts/->node ts %) ::ctor-ref (->localv-e ts) (get-ret-e ts)))
          nodes-e)
       ~(mapv #(get-site ts %) calls-e)
       ~(get-site ts ret-e)
       (fn [~'frame]
         ~@(let [node-inits (->> nodes-e
                              (mapv (fn [e] [(->> e (ts/->node ts) ::ctor-ref (uid->e ts) (ts/->node ts) ::fx-order)
                                             (emit-node-init ts ctor-e e env nm)])))
                 call-inits (->> calls-e
                              (mapv (fn [e] [(->> e (ts/->node ts) ::fx-order)
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
                     (::site ::join ::pure ::call ::ctor ::mklocal) (recur ts (get-child-e ts e))
                     (::bindlocal) (recur ts (->bindlocal-body-e ts e))
                     (::localref) (recur ts (->> (::ref nd) (->localv-e ts) (get-ret-e ts)))
                     (::node) (ts/asc ts e ::node-used true)
                     #_else (throw (ex-info (str "cannot emit-deps/mark on " (pr-str (::type nd))) (or nd {})))))))
        es (ts/find (mark ts e) ::node-used true)]
    (into (sorted-set) (map #(::node (ts/->node ts %))) es)))

(defn emit-fn [ts e nm]
  ((fn rec [e]
     (let [nd (get (:eav ts) e)]
       (case (::type nd)
         ::ap (map rec (get-children-e ts e))
         (::pure ::site) (rec (get-child-e ts e))
         ::comp ((or (::comp-fn nd) ->thunk) (eduction (map rec) (get-children-e ts e)))
         ::literal (::v nd)
         ::ctor `(r/ctor ~nm ~(::ctor-idx nd))
         ::mklocal (recur (get-ret-e ts (get-child-e ts e)))
         ::localref (recur (->> (::ref nd) (->localv-e ts) (get-ret-e ts))))))
   e))

(defn get-deps [sym] (-> sym resolve meta ::deps))

(defn delete-point-recursively [ts e]
  (let [ts (ts/del ts e)]
    (if-some [ce (get-children-e ts e)]
      (reduce delete-point-recursively ts ce)
      ts)))

(def pure-fns '#{clojure.core/vector clojure.core/hash-map})

(defn implode-point [ts e]              ; remove e, reparent child, keep e as id
  (let [nd (ts/->node ts e), ce (get-child-e ts e), cnd (ts/->node ts ce)]
    (-> ts (ts/del e) (ts/del ce) (ts/add (assoc cnd :db/id e, ::parent (::parent nd))) (reparent-children ce e))))

(defn wrap-point [{{::keys [->id]} :o :as ts} e wrap-nd] ; wrap e in another point `nd`, keeping order
  (let [nd (ts/->node ts e), new-e (->id)]
    (-> ts (ts/del e)
      (ts/add (merge wrap-nd (select-keys nd [:db/id ::parent])))
      (reparent-children e new-e)
      (ts/add (assoc nd :db/id new-e, ::parent e)))))

(defn analyze-electric [env {{::keys [->id]} :o :as ts}]
  (when (::print-analysis env) (prn :analysis) (run! prn (ts->reducible ts)))
  (let [pure-fn? (fn pure-fn? [nd] (and (= ::literal (::type nd)) (pure-fns (::v nd))))
        collapse-ap-with-only-pures
        (fn collapse-ap-with-only-pures [ts]
          (reduce (fn [ts ap-uid]
                    (let [ap-e (uid->e ts ap-uid), ce (get-children-e ts ap-e)]
                      (when (::print-ap-collapse env) (prn :ap-collapse) (run! prn (ts->reducible ts)))
                      (if (every? #(= ::pure (::type (ts/->node ts (get-ret-e ts %)))) ce)
                        (if (pure-fn? (->> ce first (get-ret-e ts) (get-child-e ts) (ts/->node ts)))
                          ;; (ap (pure vector) (pure 1) (pure 2)) -> (pure (comp-with list vector 1 2))
                          (-> (reduce (fn [ts ce]
                                        (let [pure-e (get-ret-e ts ce)]
                                          (implode-point ts pure-e)))
                                (ts/asc ts ap-e ::type ::comp, ::comp-fn list*) ce)
                            (wrap-point ap-e {::type ::pure}))
                          ;; (ap (pure x) (pure y) (pure z)) -> (ap (pure (comp-with ->call x y z)))
                          (let [pure-e (->id), comp-e (->id)]
                            (reduce (fn [ts e]
                                      (let [ce (->> e (get-ret-e ts) (get-child-e ts))
                                            cnd (ts/->node ts ce), newe (->id)]
                                        (-> ts
                                          (ts/add (assoc cnd :db/id newe, ::parent comp-e))
                                          (reparent-children ce newe)
                                          (delete-point-recursively e))))
                              (-> ts
                                (ts/add {:db/id pure-e, ::parent ap-e, ::type ::pure})
                                (ts/add {:db/id comp-e, ::parent pure-e, ::type ::comp}))
                              ce)))
                        ts)))
            ts (eduction (map #(e->uid ts %)) (ts/find ts ::type ::ap))))
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
                                (::site ::join ::pure ::call ::mklocal) (recur ts (get-child-e ts e))
                                (::bindlocal) (recur ts (->bindlocal-body-e ts e))
                                (::ctor) (if (::ctor-idx nd)
                                           ts
                                           (recur (ts/asc ts e ::ctor-idx (->ctor-idx)) (get-child-e ts e)))
                                (::localref) (recur ts (->> (::ref nd) (->localv-e ts) (get-ret-e ts)))
                                #_else (throw (ex-info (str "cannot mark-used-ctors on " (pr-str (::type nd))) (or nd {})))))))
        ts (-> ts (compute-effect-order (get-root-e ts)) (mark-used-ctors (get-root-e ts)))
        ctors-uid (mapv #(e->uid ts %) (get-ordered-ctors-e ts))
        has-node? (fn has-node? [ts uid] (ts/find ts ::ctor-ref uid))
        ensure-node (fn ensure-node [ts uid]
                      (let [ctor-uid (e->uid ts (find-ctor-e ts (uid->e ts uid)))]
                        (cond-> ts (not (has-node? ts uid))
                                (ts/add {:db/id (->id) ::ctor-node ctor-uid, ::ctor-ref uid}))))
        ensure-free-node (fn ensure-free-node [ts uid ctor-uid]
                           (cond-> ts (not (ts/find ts ::ctor-free ctor-uid, ::closed-ref uid))
                                   (ts/add {:db/id (->id) ::ctor-free ctor-uid, ::closed-ref uid, ::closed-over ::node})))
        ensure-free-free (fn ensure-free-free [ts uid ctor-uid]
                           (cond-> ts (not (ts/find ts ::ctor-free ctor-uid, ::closed-ref uid))
                                   (ts/add {:db/id (->id) ::ctor-free ctor-uid, ::closed-ref uid, ::closed-over ::free})))
        ensure-free-frees (fn ensure-free-frees [ts uid ctors-uid]
                            (reduce (fn [ts ctor-uid] (ensure-free-free ts uid ctor-uid)) ts ctors-uid))
        order-nodes (fn order-nodes [ts]
                      (reduce (fn [ts nodes-e]
                                (let [->idx (->->id)]
                                  (reduce (fn [ts e] (ts/asc ts e ::node-idx (->idx)))
                                    ts (sort-by #(->> % (ts/->node ts) ::ctor-ref (uid->e ts) (ts/->node ts) ::fx-order)
                                         nodes-e))))
                        ts (-> ts :ave ::ctor-node vals)))
        order-frees (fn order-frees [ts]
                      (reduce (fn [ts frees-e]
                                (let [->idx (->->id)]
                                  (reduce (fn [ts e] (ts/asc ts e ::free-idx (->idx)))
                                    ts (sort-by #(::fx-order (ts/->node ts %)) frees-e))))
                        ts (-> ts :ave ::ctor-free vals)))
        unlink (fn [ts e]
                 (-> ts (reparent-children e (::parent (ts/->node ts e))) (ts/del e)))
        inline-nodes (fn inline-nodes [ts]
                       (reduce (fn [ts mklocal-uid]
                                 (let [mklocal-nd (ca/is (ts/->node ts (uid->e ts mklocal-uid)) (comp #{::mklocal} ::type))
                                       localrefs-e (mapv #(uid->e ts %) (::used-refs mklocal-nd))
                                       localref-e (first (ca/check #(= 1 (count %)) localrefs-e {:refs localrefs-e, :mklocal-nd mklocal-nd}))
                                       localv-e (->localv-e ts mklocal-uid), localv-nd (ts/->node ts localv-e)
                                       site (get-site ts (get-ret-e ts localv-e))]
                                   (-> ts
                                     (ts/asc localref-e ::type ::site)
                                     (ts/asc localref-e ::site site)
                                     (ts/asc localv-e ::parent localref-e)
                                     (unlink (:db/id mklocal-nd))
                                     (unlink (::parent localv-nd)))))
                         ts (->> ts :ave ::used-refs vals (reduce into)
                              (mapv #(e->uid ts %))
                              (remove #(has-node? ts %)))))
        in-a-call? (fn in-a-call? [ts ref-e mklocal-e]
                     (loop [e (::parent (ts/->node ts ref-e))]
                       (when-let [nd (ts/->node ts e)]
                         (case (::type nd)
                           ::call e
                           ::ctor nil
                           #_else (when (not= e mklocal-e) (recur (::parent nd)))))))
        seen (volatile! #{})
        reroute-local-aliases (fn reroute-local-aliases [ts]
                                (reduce (fn [ts bl-e]
                                          (let [v-e (get-child-e ts bl-e), v-nd (ts/->node ts v-e)]
                                            (if (= ::localref (::type v-nd))
                                              (let [bl-nd (ts/->node ts bl-e)]
                                                (reduce (fn [ts lr-e] (ts/asc ts lr-e ::ref (::ref v-nd)))
                                                  ts
                                                  (ts/find ts ::type ::localref, ::ref (::ref bl-nd))))
                                              ts)))
                                  ts (ts/find ts ::type ::bindlocal)))
        handle-let-refs (fn handle-let-refs [ts e] ; nodes and frees (closed over)
                          (let [nd (ts/->node ts e)]
                            (case (::type nd)
                              (::literal ::var ::lookup ::node) ts
                              (::ap ::comp) (reduce handle-let-refs ts (get-children-e ts e))
                              (::site ::join ::pure ::ctor ::call ::mklocal) (recur ts (get-child-e ts e))
                              (::bindlocal) (recur ts (->bindlocal-body-e ts e))
                              (::localref)
                              (let [mklocal-uid (::ref nd), mklocal-e (uid->e ts mklocal-uid)
                                    mklocal-nd (ts/->node ts mklocal-e)
                                    ctors-e (loop [ac '(), e (::parent (ts/->node ts e))]
                                              (if (= mklocal-e e)
                                                ac
                                                (let [nd (ts/->node ts e)]
                                                  (recur (cond-> ac (= ::ctor (::type nd)) (conj e)) (::parent nd)))))
                                    ctors-uid (mapv #(e->uid ts %) ctors-e)
                                    localv-e (->localv-e ts mklocal-uid)
                                    ts (if-some [call-e (in-a-call? ts e mklocal-e)]
                                         (-> ts (ts/upd mklocal-e ::in-call #(conj (or % #{}) (e->uid ts call-e)))
                                           (ensure-node mklocal-uid))
                                         ts)
                                    ts (if (seq ctors-e) ; closed over
                                         (-> ts (ensure-node mklocal-uid)
                                           (ensure-free-node mklocal-uid (first ctors-uid))
                                           (ensure-free-frees mklocal-uid (next ctors-uid)))
                                         (cond-> (ts/upd ts mklocal-e ::used-refs #(conj (or % #{}) (::uid nd)))
                                           (or (= 1 (count (::used-refs mklocal-nd))) ; before inc, now it's 2
                                             (when-some [pt-e (find-sitable-point-e ts e)]
                                               (not= (get-site ts pt-e)
                                                 (get-site ts (get-ret-e ts localv-e)))))
                                           (ensure-node mklocal-uid)))]
                                (or (and (@seen mklocal-uid) ts)
                                  (do (vswap! seen conj mklocal-uid)
                                      (recur ts (get-ret-e ts localv-e)))))
                              #_else (throw (ex-info (str "cannot handle-let-refs on " (::type nd)) (or nd {}))))))
        ->call-idx (let [mp (zipmap ctors-uid (repeatedly ->->id))]
                     (fn ->call-idx [ctor-uid] ((get mp ctor-uid))))
        seen (volatile! #{})
        mark-used-calls (fn mark-used-calls [ts ctor-e e]
                          (if (@seen e)
                            ts
                            (let [nd (ts/->node ts e)]
                              (vswap! seen conj e)
                              (case (::type nd)
                                (::literal ::var ::lookup ::node ::ctor) ts
                                (::ap ::comp) (reduce #(mark-used-calls % ctor-e %2) ts (get-children-e ts e))
                                (::site ::join ::pure ::mklocal) (recur ts ctor-e (get-child-e ts e))
                                (::bindlocal) (recur ts ctor-e (->bindlocal-body-e ts e))
                                (::call) (if (::call-idx nd)
                                           ts
                                           (-> (mark-used-calls ts ctor-e (get-child-e ts e))
                                             (ts/asc e ::call-idx (->call-idx (e->uid ts ctor-e)))
                                             (ts/asc e ::ctor-call (::uid (ts/->node ts ctor-e)))))
                                (::let) (recur ts ctor-e (->bindlocal-body-e ts e))
                                (::localref) (let [nx-e (->> (::ref nd) (->localv-e ts) (get-ret-e ts))]
                                               (recur ts (find-ctor-e ts nx-e) nx-e))
                                #_else (throw (ex-info (str "cannot mark-used-calls on " (::type nd)) (or nd {})))))))
        mark-used-calls2 (fn [ts]
                           (reduce (fn [ts ctor-e] (mark-used-calls ts ctor-e (get-ret-e ts (get-child-e ts ctor-e))))
                             ts (->> ts :ave ::ctor-idx vals (reduce into))))
        ts (-> ts mark-used-calls2 reroute-local-aliases (handle-let-refs (get-root-e ts))
             inline-nodes order-nodes order-frees collapse-ap-with-only-pures)]
    (when (::print-db env) (prn :db) (run! prn (ts->reducible ts)))
    ts))

(defn compile* [nm env ts]
  (let [ts (analyze-electric env ts)
        ret `(fn
               ([] {0 (r/ctor ~nm 0)})
               ([idx#]
                (case idx#
                  ~@(->> (get-ordered-ctors-e ts)
                      (map #(emit-ctor ts % env nm))
                      (interleave (range))))))]
    (when (::print-source env) (fipp.edn/pprint ret))
    ret))

(defn ->ts [] (ts/->ts {::->id (->->id), ::->uid (->->id)}))

(defn compile [nm form env]
  (compile* nm env
    (analyze (expand-all env `(::ctor ~form))
      '_ env (->ts))))
