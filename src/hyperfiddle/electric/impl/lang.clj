(ns hyperfiddle.electric.impl.lang
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [cljs.analyzer]
   [contrib.data]
   [hyperfiddle.electric :as-alias e]
   [hyperfiddle.electric.impl.analyzer :as ana]
   [hyperfiddle.electric.impl.expand :as expand]
   [hyperfiddle.electric.impl.ir :as ir]
   [hyperfiddle.electric.impl.ir-utils :as ir-utils]
   [hyperfiddle.electric.impl.runtime :as r]
   [hyperfiddle.electric.debug :as dbg]
   [hyperfiddle.rcf :refer [tests]]))

(def ^{::type ::node, :doc "for loop/recur impl"} rec)
(def ^{::type ::node, :doc "for runtime arity check"} %arity)
(def ^{::type ::node, :doc "for runtime varargs"} %args)
(def ^{::type ::node, :doc "for self-recur"} %closure)
(def ^{::type ::node, :doc "for try/catch"} exception)
(def ^{::type ::node, :doc "for case"} %case-test)
(def ^{::type ::node, :doc "In a `catch` block, bound by the runtime to the current stacktrace. An Electric stacktrace is an ExceptionInfo. Use `hyperfiddle.electric.debug/stack-trace` to get a string representation."}
  trace {:fn (fn [_frame _vars _env] (r/pure nil)),
         :get-used-nodes #(), :var-name `trace
         :noutput 0, :ninput 0, :nvariable 0, :nsource 0, :ntarget 0, :dynamic '[], :nconstant 0})

(def arg-sym
  (map (comp symbol
         (partial intern *ns*)
         (fn [i]
           (with-meta (symbol (str "%" i))
             {::type ::node})))
    (range)))
;; pre-define the first 20 for e/fn varargs expansion
(def ^{::type ::node} %0)
(def ^{::type ::node} %1)
(def ^{::type ::node} %2)
(def ^{::type ::node} %3)
(def ^{::type ::node} %4)
(def ^{::type ::node} %5)
(def ^{::type ::node} %6)
(def ^{::type ::node} %7)
(def ^{::type ::node} %8)
(def ^{::type ::node} %9)
(def ^{::type ::node} %10)
(def ^{::type ::node} %11)
(def ^{::type ::node} %12)
(def ^{::type ::node} %13)
(def ^{::type ::node} %14)
(def ^{::type ::node} %15)
(def ^{::type ::node} %16)
(def ^{::type ::node} %17)
(def ^{::type ::node} %18)
(def ^{::type ::node} %19)

(defn get-configs-to-compile [conf lang]
  (into #{}
    (comp
      (keep (fn [[peer peer-lang]] (when (= lang peer-lang) peer)))
      (mapcat (fn [peer] (into [] (comp (filter (fn [current] (or (not (::only conf)) (get (::only conf) current))))
                                    (map (fn [current] (assoc conf ::me peer, ::current current))))
                           (keys (::peers conf))))))
    (::peers conf)))

(tests
  (get-configs-to-compile {::peers {:client :clj :server :cljs}, ::current :client} :cljs)
  := #{{::peers {:client :clj :server :cljs}, ::me :server, ::current :client}
       {::peers {:client :clj :server :cljs}, ::me :server, ::current :server}}

  (get-configs-to-compile {::peers {:client :clj :server :clj}, ::current :client} :clj)
  := #{{::peers {:client :clj :server :clj}, ::me :client, ::current :client}
       {::peers {:client :clj :server :clj}, ::me :client, ::current :server}
       {::peers {:client :clj :server :clj}, ::me :server ::current :client}
       {::peers {:client :clj :server :clj}, ::me :server ::current :server}}

  (get-configs-to-compile {::peers {:client :clj :server :clj}, ::current :client, ::only #{:client}} :clj)
  := #{{::peers {:client :clj :server :clj}, ::me :client, ::current :client, ::only #{:client}}
       {::peers {:client :clj :server :clj}, ::me :server, ::current :client, ::only #{:client}}}
  )

(defn mksym [x & xs]
  (if (or (symbol? x) (keyword? x))
    (symbol (namespace x) (apply str (name x) (map name (flatten xs))))
    (symbol (apply str (name x) (map name (flatten xs))))))
(defn as-node [o] (vary-meta o assoc ::type ::node))
(defn node? [mt] (-> mt ::type #{::node}))
(defn as-node-signifier [o] (vary-meta o assoc ::type ::node-signifier))
(defn node-signifier? [mt] (-> mt ::type #{::node-signifier}))
(defn signifier->node [sym cfg] (mksym sym "_hf_" (::me cfg) "_" (::current cfg)))

(defn with-local
  ([env sym] (with-local env sym nil))
  ([env sym v]
   (let [peer (::current env)
         i (get (::index env) peer 0)]
     (-> env
       (update ::index assoc peer (inc i))
       (update :locals update sym assoc ::pub i, ::peer peer, ::meta (cond-> (meta sym)
                                                                       (::ir/tag v) (assoc :tag (::ir/tag v))))))))

(defn find-local [sym env] (-> env :locals (get sym)))
(defn find-electric-local [sym env] (let [local (find-local sym env)] (when (::pub local) local)))

(defn causal [stmt expr]
  (ir/apply (ir/eval '{}) stmt expr))

(defn causal-publish
  ([init inst] (causal-publish init inst nil))
  ([init inst ?sym] (cond-> (ir/pub init (causal (ir/sub 1) inst)) ?sym (assoc ::form ?sym))))

(defn- ensure-seq [v] (if (seq? v) v (seq [v])))
(defn- ->case-picker-map [test-constants-coll]
  (into {} (map-indexed (fn [i v] (zipmap (ensure-seq v) (repeat i)))) test-constants-coll))

(tests
  (->case-picker-map '((1 2) 4)) := {1 0, 2 0, 4 1}
  (->case-picker-map '([a b])) := {'[a b] 0})

(defn- find-node-signifier [sym env]
  (case (get (::peers env) (::me env))
    :clj (when-some [^clojure.lang.Var vr (resolve env sym)]
           (when (-> vr meta node-signifier?)
             (symbol (-> vr .ns str) (-> vr .sym str))))
    :cljs (when-some [vr (expand/resolve-cljs env sym)]
            (when (-> vr :meta node-signifier?)
              (symbol (-> vr :name str)))))) ; there's `:ns` but `:name` already contains the ns (?)

(defn- find-node [sym env]
  (case (get (::peers env) (::me env))
    :clj (when-some [^clojure.lang.Var vr (resolve env sym)]
           (when (-> vr meta node?)
             (symbol (-> vr .ns str) (-> vr .sym str))))
    :cljs (when-some [vr (expand/resolve-cljs env sym)]
            (when (-> vr :meta node?)
              (symbol (-> vr :name str)))))) ; there's `:ns` but `:name` already contains the ns (?)

(declare analyze-me analyze-them)

(defn get-them [env] (-> env ::peers keys set (disj (::current env)) first))
(defn toggle [env] (assoc env ::current (get-them env)))

(tests
  (toggle {::peers {:client :cljs, :server :clj} ::current :server})
  := {::peers {:client :cljs, :server :clj} ::current :client})

(defn fail!
  ([env msg] (fail! env msg {}))
  ([env msg data] (throw (ex-info (str "in" (some->> (::def env) (str " ")) ": " (-> env ::last peek pr-str) "\n" msg)
                           (merge {:form (-> env ::last pop peek) :in (::def env) :for ((juxt ::me ::current) env)} data)))))

(defn cannot-resolve! [env form]
  (fail! env (str "I cannot resolve " "`"form"`"
               (when-let [them (get-them env)]
                 (let [site (name them)]
                   (str ", maybe it's defined only on the " site "?"
                     \newline "If `" form "` is supposed to be a macro, you might need to :refer it in the :require-macros clause."))))
    {:locals (keys (:locals env))}))

(defn analyze-binding [env bs f & args]
  ((fn rec [env bs ns]
     (if-some [[node form & bs] bs]
       (let [init (analyze-me env form)
             inst (rec (update env ::index update (::current env) (fnil inc 0)) bs (conj ns node))]
         (if (::non-causal env)
           (cond-> (ir/pub init inst) node (assoc ::form node))
           (causal-publish init inst node)))
       (reduce-kv (fn [res i n]
                    (if-some [qualified-sym (find-node-signifier n env)]
                      (ir/bind (signifier->node qualified-sym env) (- (count ns) i) res)
                      (if-some [qualified-sym (find-node n env)]
                        (ir/bind qualified-sym (- (count ns) i) res)
                        (fail! env (str "Not an electric e/def: " n) {:symbol n}))))
         (apply f env args) ns)))  env (seq bs) []))

(defn analyze-case [env expr clauses]
  (let [clauses (vec clauses)
        [default-clause clauses] (if (even? (count clauses))
                                   [`(r/case-default-throw %case-test) clauses]
                                   [(peek clauses) (pop clauses)])
        picker-map (->case-picker-map (take-nth 2 clauses))]
    (analyze-binding env [`%case-test expr]
      (fn [env]
        (ir/variable
          (apply ir/apply (ir/eval `r/pick-case-branch) (ir/eval (list 'quote picker-map))
            (analyze-me env `%case-test)
            (analyze-me env (list ::closure default-clause {::dbg/type :case-default}))
            (mapv (fn [[test form]]
                    (analyze-me env `(::closure ~form {::dbg/type :case-clause
                                                       ::dbg/meta ~(meta form)})))
              (partition 2 clauses))))))))

(defn cat-them [& xs] (into [] cat xs))
(defn ->them [base deps] (assoc base ::ir/deps deps))

(defn analyze-them-case [env expr clauses]
  (let [clauses (vec clauses)
        [default-clause clauses] (if (even? (count clauses))
                                   [`(r/case-default-throw %case-test) clauses]
                                   [(peek clauses) (pop clauses)])]
    (if (::in-interop-fn? env)
      (apply cat-them (analyze-them env expr) (eduction (take-nth 2) (map #(analyze-them env %)) (rest clauses)))
      (apply cat-them [(assoc ir/source ::form (list 'case expr))]
        (analyze-them env expr)
        (analyze-them env (list ::closure default-clause {::dbg/type :case-default}))
        (eduction (partition-all 2) (map (fn [[test form]]
                                           (analyze-them env `(::closure ~form {::dbg/type :case-clause
                                                                                ::dbg/meta ~(meta form)}))))
          clauses)))))

(defn ns-qualify [node] (if (namespace node) node (symbol (str *ns*) (str node))))

(tests
  (ns-qualify 'foo) := `foo
  (ns-qualify 'a/b) := 'a/b)

(defn qualify-sym-in-var-node "If ast node is `:var`, update :form to be a fully qualified symbol" [env ast]
  (if (and (= :var (:op ast)) (not (-> ast :env :def-var)))
    (assoc ast :form (case (get (::peers env) (::current env))
                       :clj  (symbol (str (:ns (:meta ast))) (str (:name (:meta ast))))
                       :cljs (:name (:info ast))))
    ast))

(defn ->meta [o env] (merge (::meta (find-electric-local o env)) (meta o)))

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
                          (swap! refered-lexical assoc (with-meta form (->meta form env))
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
        form            (case (get (::peers env) (::current env))
                          :clj  (-> (ana/analyze-clj env form)
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

(defn- expand-try [args env]
  (let [[forms catches finally]
        (reduce
          (fn [[forms catches finally] form]
            (case finally
              nil (let [[op & args] (when (seq? form) form)]
                    (case op
                      catch   [forms (conj catches args) finally]
                      finally [forms catches (vec args)]
                      (case catches
                        [] [(conj forms form) catches finally]
                        (fail! env "Invalid try block - unrecognized clause."))))
              (fail! env "Invalid try block - finally must be in final position.")))
          [[] [] nil] args)
        body `(::closure (do ~@forms) {::dbg/type :try})]
    (expand/all env
      `(new ~(reduce
               (fn [r f] `(r/latest-first ~r (::closure ~f {::dbg/type :finally})))
               (case catches
                 [] body
                 `(r/bind r/recover
                    (some-fn
                      ~@(map (fn [[c s & body]]
                               (let [f `(partial (::inject exception)
                                          (::closure (let [~s (dbg/unwrap exception)]
                                                       (binding [trace exception]
                                                         ~@body))
                                                     {::dbg/type :catch}))]
                                 (case c
                                   (:default Throwable)
                                   `(r/clause ~f)
                                   `(r/clause ~f ~c)))) catches))
                    ~body)) finally)))))

(defn ->class-method-call [clazz method method-args env]
  (apply ir/apply (assoc (ir/eval (let [margs (repeatedly (count method-args) gensym)]
                                    `(fn [~@margs] (. ~clazz ~method ~@margs))))
                    ::dbg/action :static-call, ::dbg/target clazz, ::dbg/method method)
    (mapv #(analyze-me env %) method-args)))

(defn ->obj-method-call [o method method-args env]
  (apply ir/apply (assoc (ir/eval (let [margs (repeatedly (count method-args) gensym)
                                        oo (with-meta (gensym "o") (->meta o env))]
                                    `(fn [~oo ~@margs] (. ~oo ~method ~@margs))))
                    ::dbg/action :call, ::dbg/target o, ::dbg/method method)
    (analyze-me env o)
    (mapv #(analyze-me env %) method-args)))

(defn bound-js-fn
  "Given a js global resolving to a function (e.g js/alert, js/console.log required-js-ns/js-fn), ensures it
  is called under the correct `this` context."
  [sym]
  (let [fields (str/split (name sym) #"\.")]
    `(.bind ~sym ~(symbol (namespace sym)
                    (if (seq (rest fields))
                      (str/join (interpose '. (butlast fields)))
                      "globalThis")))))

(defn- class-constructor-call? [env f]
  (and (symbol? f) (not (or (find-local f env) (find-node-signifier f env) (find-node f env)))))

(defn with-interop-locals [env syms] (update env :locals merge (zipmap syms (repeat {}))))

(defn resolve-static-field [sym]
  (when-some [ns (some-> (namespace sym) symbol)]
    (when-some [cls (resolve ns)]
      (when (class? cls)
        (clojure.lang.Reflector/getField cls (name sym) true)))))

(defn source-map
  ([env debug-info]
   (let [file (if (:js-globals env) (:file (:meta (:ns env)))
                  (some-> env :ns find-ns meta :file))]
     (if (some? file)
       (merge {:file file} debug-info)
       (or debug-info {})))))

(defn keep-if [pred v] (when (pred v) v))

(defn store [env form]
  (if (::last env)
    (update env ::last #(conj (pop %) form))
    (assoc env ::last (conj (clojure.lang.PersistentQueue/EMPTY) nil form))))

(defn pr-str-short [o]
  (if (and (seq? o) (seq o))
    (apply str `("(" ~@(interpose " " (take 2 o)) " .. )"))
    (str o)))

(defn analyze-me [env form]
  (when (::trace env) (prn :analyze (if (and (seq? form) (seq form)) (pr-str-short form) form)))
  (let [env (store env form)]
    (cond
      (and (seq? form) (seq form))
      (let [[op & args] form]
        (case op
          (let*) ((fn rec [env bs]
                    (if-some [[s i & bs] bs]
                      (let [v (analyze-me env i)]
                        (cond-> (if (::non-causal env)
                                  (cond-> (ir/pub v (rec (with-local env s v) bs)) s (assoc ::form s))
                                  (causal-publish v (rec (with-local env s v) bs) s))
                          (::ir/tag v) (assoc ::ir/tag (::ir/tag v))))
                      (analyze-me env (cons `do (next args)))))
                  env (seq (first args)))

          (do) (if-some [[x & xs] args]
                 ((fn rec [x xs]
                    (let [r (analyze-me env x)]
                      (if-some [[y & ys] xs]
                        (causal r (rec y ys))
                        r)))  x xs)
                 (ir/eval nil))

          (case) (analyze-case env (first args) (rest args))
          (if) (let [[test then else] args] (analyze-case env test (list '(nil false) else then)))

          (quote) (ir/eval (list 'quote (first args)))

          (js*) (if-some [[f & args] args]
                  (apply ir/apply (assoc (ir/eval (let [args (repeatedly (count args) gensym)]
                                                    `(fn [~@args] (~'js* ~f ~@args))))
                                    ::dbg/action :js-call)
                    (mapv #(analyze-me env %) args))
                  (fail! env "Wrong number of arguments - js*" ))

          (fn*) (let [[env arities] (if (symbol? (first args))
                                      [(update env :locals assoc (first args) true) (next args)]
                                      [env args])
                      env (assoc env ::in-interop-fn? true)]
                  (doseq [[bs & body] arities] ; checks for invalid calls, may throw
                    (analyze-me (with-interop-locals env bs) (cons 'do body)))
                  (let [[form refs] (closure env (cons 'fn* args))]
                    (apply ir/apply (assoc (ir/eval form)
                                      ::dbg/action :fn-call, ::dbg/name (keep-if symbol? (first args)))
                      (eduction (map #(analyze-me env %)) refs))))

          ;; (letfn* [foo (fn* foo ([x] x))] ...)
          (letfn*) (let [fnenv (with-interop-locals env (take-nth 2 (first args)))]
                     (doseq [[_fn* _nm & arities] (take-nth 2 (nfirst args))] ; checks for invalid calls, may throw
                       (doseq [[bs & body] arities]
                         (analyze-me (with-interop-locals fnenv bs) (cons 'do body))))
                     (let [[bs & body] args]
                       (recur env (expand/all env `(let [~(vec (take-nth 2 bs)) (::letfn ~bs)] ~@body)))))

          (::letfn) (let [bs (first args), [form refs] (closure env `(letfn* ~bs ~(vec (take-nth 2 bs))))]
                      (apply ir/apply (assoc (ir/eval form) ::dbg/type :letfn) (mapv #(analyze-me env %) refs)))

          (set!) (when-not (::in-interop-fn? env)
                   (recur env (expand/all env `((fn [v#] (set! ~(nth form 1) v#)) ~(nth form 2)))))

          (new) (if-some [[f & args] args]
                  (if (class-constructor-call? env f)
                    (cond-> (apply ir/apply (ir/eval (let [args (repeatedly (count args) gensym)] ; class constructor call
                                                       `(fn [~@args] (new ~f ~@args))))
                              (mapv #(analyze-me env %) args))
                      (= "js" (namespace f)) (assoc ::ir/tag 'js))
                    (analyze-binding env ; electric join
                      (list* `%closure f `%arity (count args) (interleave arg-sym args))
                      (fn [env]
                        #_(ir/variable (ir/sub (+ 2 (count args))))
                        (analyze-binding env [`%args `[~@(take (count args) arg-sym)]]
                          (fn [_] (ir/variable (ir/sub (+ 3 (count args)))))))))
                  (fail! env "Wrong number of arguments - new" {}))

          ;; (. java.time.Instant now)
          ;; (. java.time.Instant ofEpochMilli 1)
          ;; (. java.time.Instant (ofEpochMilli 1))
          ;; (. java.time.Instant EPOCH)
          ;; (. java.time.Instant -EPOCH)
          ;; (. i1 isAfter i2)
          ;; (. i1 (isAfter i2))
          ;; (. pt x)
          ;; (. pt -x)
          (.) (if (and (= :clj (get (::peers env) (::current env)))
                    (symbol? (first args)) (class? (resolve env (first args))))
                (if (seq? (second args))
                  (let [[clazz [method & method-args]] args]
                    (->class-method-call clazz method method-args env))
                  (let [[clazz x & xs] args]
                    (if (seq xs)
                      (->class-method-call clazz x xs env)
                      (ir/eval form))))
                (if (seq? (second args))
                  (let [[o [method & method-args]] args]
                    (->obj-method-call o method method-args env))
                  (let [[o x & xs] args]
                    (if (seq xs)
                      (->obj-method-call o x xs env)
                      (ir/apply (ir/eval
                                  (let [oo (with-meta (gensym "o") (->meta o env))]
                                    `(fn [~oo] (. ~oo ~x)))) (analyze-me env o))))))

          (throw) (recur env (expand/all env `(r/fail ~(first args) trace)))

          (try) (recur env (expand-try args env))

          (loop*) (let [[bs & body] args]
                    (recur env (expand/all env
                                 `(binding [rec (::closure (let [~@(interleave (take-nth 2 bs) arg-sym)] ~@body))]
                                    (new rec ~@(take-nth 2 (next bs)))))))

          (recur) (recur env `(new rec ~@args))

          (def) (let [[sym v] args]
                  (if (or (find-node sym env) (find-node-signifier sym env))
                    (fail! env "Cannot `def` a reactive var" {:var sym})
                    (when-not (::in-interop-fn? env)
                      (recur env (expand/all env
                                   (if (= :cljs (get (::peers env) (::current env)))
                                     (let [ns (-> env :ns :name)]
                                       (swap! @(requiring-resolve 'cljs.env/*compiler*)
                                         assoc-in [:cljs.analyzer/namespaces ns :defs sym] {:name sym})
                                       `(set! ~sym ~v))
                                     `((fn [x#] (def ~sym x#)) ~v)))))))

          (::lift) (ir/lift (analyze-me env (first args)))

          (::closure) (let [[form debug-info] args]
                        (merge (ir/constant (analyze-me env (expand/all env form))) debug-info))

          (::toggle) (let [[peer debug-info & body] args]
                       (if (= peer (::current env))
                         (recur env (cons 'do body))
                         (->them {::ir/op ::ir/input, ::dbg/meta (source-map env debug-info), ::dbg/type :toggle}
                           (analyze-them (assoc env ::current peer) (cons 'do body)))))

          (::inject) (ir/inject (let [sym (first args)]
                                  (if-some [qualified-sym (find-node-signifier sym env)]
                                    (signifier->node qualified-sym env)
                                    (if-some [qualified-sym (find-node sym env)]
                                      qualified-sym
                                      (fail! env (str "Not a node: " sym) {:sym sym})))))

          (binding) (if (::in-interop-fn? env)
                      (run! #(analyze-me env %) (eduction cat [(eduction (take-nth 2) (nfirst args)) (next args)]))
                      (analyze-binding env (first args) analyze-me (cons `do (next args))))

          (clojure.core/unquote-splicing) (->them {::ir/op ::ir/input , ::dbg/meta (meta form), ::dbg/type :toggle}
                                            (analyze-them (toggle env) (next form)))

          #_else (apply ir/apply
                   (if-let [resolved (and (symbol? op) (= :cljs (get (::peers env) (::current env)))
                                       (->> (expand/resolve-cljs env op) (keep-if (comp '#{js} :ns)) :name))]
                     (assoc (ir/eval (bound-js-fn resolved)) ::ir/tag 'js)
                     (assoc (analyze-me env op) ::dbg/name op ::dbg/file (:file (meta op)) ::dbg/line (:line (meta op))))
                   (mapv #(analyze-me env %) args))))

      (node-signifier? (meta form))
      (ir/node (ns-qualify form))

      (instance? cljs.tagged_literals.JSValue form)
      (let [o (.-val ^cljs.tagged_literals.JSValue form)]
        (if (map? o)
          (recur env (expand/all env (cons 'cljs.core/js-obj (into [] (mapcat (fn [[k v]] [(name k) v])) o))))
          (recur env (expand/all env (cons 'array o)))))

      (symbol? form)
      (if-some [local (find-local form env)]
        (if (::pub local)
          (let [debug-info {::dbg/name (with-meta form nil), ::dbg/scope :lexical, ::dbg/meta (meta form)}]
            (if (= (::peer local) (::current env))
              (merge (ir/sub (- (get (::index env) (::current env)) (::pub local))) debug-info)
              (->them (assoc debug-info ::ir/op ::ir/input) (analyze-them env (list ::toggle (::peer local) form)))))
          (ir/eval form))
        (if-some [qualified-sym (find-node-signifier form env)]
          (let [node (signifier->node qualified-sym env)]
            (if (case (get (::peers env) (::current env))
                  :clj (resolve node)
                  :cljs (expand/resolve-cljs env node))
              (assoc (ir/node node) ::dbg/name qualified-sym, ::dbg/scope :dynamic)
              (cannot-resolve! env form)))
          (if-some [qualified-sym (find-node form env)]
            (assoc (ir/node qualified-sym) ::dbg/name qualified-sym, ::dbg/scope :dynamic)
            (if (case (get (::peers env) (::current env))
                  :clj (or (resolve-static-field form) (resolve form))
                  :cljs (expand/resolve-cljs env form))
              (ir/eval form)
              (cannot-resolve! env form)))))

      (vector? form) (recur env (cons `vector form))
      (map? form) (recur env (if-let [m (meta form)]
                               (list `with-meta (cons `hash-map (eduction cat form)) m)
                               (cons `hash-map (eduction cat form))))
      (set? form) (recur env (cons `hash-set form))

      :else (ir/eval form))))

(defn analyze-them [env form]
  (let [env (store env form)]
    (cond
      (and (seq? form) (seq form))
      (let [[op & args] form]
        (case op
          (let*) (loop [env env, bs (seq (first args)), ret []]
                   (if-some [[s i & bs] bs]
                     (recur (with-local env s) bs (conj ret (analyze-them env i)))
                     (apply cat-them (conj ret (analyze-them env (cons `do (next args)))))))

          (do) (apply cat-them (eduction (map #(analyze-them env %)) args))

          (case) (analyze-them-case env (first args) (rest args))
          (if) (let [[test then else] args] (analyze-them-case env test (list '(nil false) else then)))

          (quote) nil
          (js*) (apply cat-them (eduction (map #(analyze-them env %)) args))

          (fn*) (let [[env arities] (if (symbol? (first args))
                                      [(update env :locals assoc (first args) true) (next args)]
                                      [env args])
                      env (assoc env ::in-interop-fn? true)]
                  (apply cat-them (eduction (map (fn [[bs & body]]
                                                   (analyze-them (with-interop-locals env bs)
                                                     (cons 'do body))))  arities)))

          ;; (letfn* [foo (fn* foo ([x] x))] ...)
          (letfn*)
          (let [[fns & body] args
                fnenv (-> env (with-interop-locals (take-nth 2 fns))
                        (assoc ::in-interop-fn? true))
                fns-ret (apply cat-them (eduction (take-nth 2) (map #(analyze-them fnenv %)) (next fns)))
                env (with-interop-locals env (take-nth 2 fns))]
            (cat-them fns-ret (analyze-them env (cons 'do body))))

          (set!) (cat-them (analyze-them env (nth form 1)) (analyze-them env (nth form 2)))

          (new) (if-some [[f & fargs] args]
                  (if (or (::in-interop-fn? env) (class-constructor-call? env f))
                    (apply cat-them (eduction (map #(analyze-them env %)) fargs))
                    (apply cat-them
                      [(assoc ir/source ::form (list 'new (cond-> f (seq? f) first)))]
                      (eduction (map #(analyze-them env %)) args)))
                  (fail! env "Wrong number of arguments - new" {}))

          (.) (apply cat-them (eduction (map #(analyze-them env %)) args))

          (throw) (recur env (expand/all env `(r/fail ~(first args) trace)))

          (try) (recur env (expand-try args env))

          (loop*) (let [[bs & body] args]
                    (recur env (expand/all env
                                 `(binding [rec (::closure (let [~@(interleave (take-nth 2 bs) arg-sym)] ~@body))]
                                    (new rec ~@(take-nth 2 (next bs)))))))

          (recur) (recur env `(new rec ~@args))

          (def) (recur env (nth form 2))

          (::lift) (recur env (first args))

          (::closure) [(->them {::ir/op ::ir/target, ::form :closure} (analyze-them env (first args)))]

          (::toggle) (let [[peer debug-info & body] args]
                       (if (= peer (::me env))
                         [(ir/output (analyze-me (assoc env ::current peer) (cons 'do body)))]
                         (analyze-them env (cons 'do body))))

          (::inject) nil

          (binding) (let [[bs & body] args]
                      (apply cat-them (eduction cat (map #(analyze-them env %))
                                        [(eduction (take-nth 2) (next bs)) body])))

          (clojure.core/unquote-splicing) [(ir/output (analyze-me (toggle env) (next form)))]

          #_else (apply cat-them (eduction (map #(analyze-them env %)) form))))

      (instance? cljs.tagged_literals.JSValue form)
      (let [o (.-val ^cljs.tagged_literals.JSValue form)]
        (if (map? o)
          (recur env (expand/all env (cons 'cljs.core/js-obj (into [] (mapcat (fn [[k v]] [(name k) v])) o))))
          (recur env (expand/all env (cons 'array o)))))

      (symbol? form)
      (if-some [local (find-local form env)]
        (let [debug-info {::dbg/name (with-meta form nil), ::dbg/scope :lexical, ::dbg/meta (meta form)}]
          (when (and (::pub local) (= (::me env) (::peer local)))
            [(ir/output (merge (analyze-me (assoc env ::current (::me env)) form) debug-info))]))
        (when-some [qualified-sym (find-node-signifier form env)]
          (let [node (signifier->node qualified-sym env)]
            (if (case (get (::peers env) (::me env))
                  :clj (resolve node)
                  :cljs (expand/resolve-cljs env node))
              [(assoc (ir/node node) ::dbg/name qualified-sym, ::dbg/scope :dynamic)]
              (cannot-resolve! env form)))))

      (vector? form) (recur env (cons `vector form))
      (map? form) (recur env (if-let [m (meta form)]
                               (list `with-meta (cons `hash-map (eduction cat form)) m)
                               (cons `hash-map (eduction cat form))))
      (set? form) (recur env (cons `hash-set form))

      :else nil)))

(defn analyze [env form]
  (binding [expand/*electric* true]
    (let [expanded (expand/all env form)]
      (when (::pprint-expansion env)
        (println "---" (::sym env) "EXPANSION ---")
        (pp/pprint expanded))
      (let [ret (if (= (::me env) (::current env))
                  (analyze-me env expanded)
                  (->them {::ir/op ::ir/do, ::ir/inst ir/nop} (analyze-them env expanded)))]
        (when (::pprint-ir env)
          (println "---" (::sym env) "IR ---")
          (pp/pprint (ir-utils/unwrite ret)))
        ret))))


(defn electric-only [& args]
  (throw (ex-info "I'm an electric value and you called me outside of electric." {:args args})))

(defn -def
  ([env name-sym init]
   (when-not (::has-edef? (meta *ns*))
     (alter-meta! *ns* assoc ::has-edef? true))
   (let [lang (if (:js-globals env) :cljs :clj)
         env (merge env (contrib.data/select-ns 'hyperfiddle.electric.impl.lang (meta name-sym)))
         configs (get-configs-to-compile env lang)]
     `(do (def ~(as-node-signifier name-sym) electric-only)
          ~@(for [config configs]
              (let [env (merge env config {::def name-sym})
                    sym (signifier->node name-sym env)
                    fullsym (symbol (str *ns*) (str sym))
                    _ (when (::print-defs (meta name-sym)) (prn 'defining fullsym))
                    env (assoc env ::sym sym)
                    ir (analyze env init)
                    info (r/compile name-sym ir env)]
                (list `def (as-node sym) (assoc info :var-name (list 'quote fullsym)))))))))
