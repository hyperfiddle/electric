(ns hyperfiddle.electric.impl.compiler
  (:require [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.env :as clj-env]
            [cljs.analyzer :as cljs]
            [cljs.env]
            [cljs.util]
            [hyperfiddle.electric.impl.local :as l]
            [hyperfiddle.electric.impl.runtime :as r]
            [hyperfiddle.electric.impl.ir :as ir]
            [hyperfiddle.electric.impl.env :as env]
            [hyperfiddle.electric.debug :as dbg]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.electric.impl.analyzer :as ana]
            [clojure.tools.logging :as log]
            [clojure.tools.analyzer.jvm.utils :refer [maybe-class-from-string]]
            [missionary.core :as m]
            [clojure.string :as str])
  (:import cljs.tagged_literals.JSValue
           (hyperfiddle.electric.impl.env CljsVar CljVar CljClass)))

(def ^{:dynamic true, :doc "Bound to Electric compiler env when macroexpension is managed by Electric."} *env*)
(def ^{::node ::unbound, :macro true, :doc "for loop/recur impl"} rec)
(def ^{::node ::unbound, :macro true, :doc "for runtime arity check"} %arity)

;; %1, %2 ... %n p/def generator.
;; A lazy seq of vars. Forcing the seq will intern them.
;; GG: Why are %n macros?
;;     Since they are declared in a clojure file, they must be tagged as :macro true for cljs analyzer to resolve them.
(def arg-sym 
  (map (comp symbol
         (partial intern *ns*)
         (fn [i]
           (with-meta (symbol (str "%" i))
             {:macro true ::node ::unbound})))
    (range)))

;; GG: Why is `exception` a macro?
;;     Because it's declared in a clojure file, it must be tagged as :macro true for cljs analyzer to resolve it.
(doto (def exception) (alter-meta! assoc :macro true ::node ::unbound))

(defmacro ctor-call [class arity]
  (let [args (repeatedly arity gensym)]
    `(fn [~@args] (new ~class ~@args))))

(defmacro static-call [klass method arity]
  (let [args (repeatedly arity gensym)]
    `(fn [~@args] (. ~klass ~method ~@args))))

(defmacro method-call [method arity] ; used for cljs only
  (let [args (repeatedly arity gensym)]
    `(fn [inst# ~@args] (. ^js inst# ~method ~@args))))

(defmacro field-access [field] ; used for cljs only
  `(fn [inst#] (. ^js inst# ~(symbol (str "-" (name field))))))

(defmacro js-call [template arity]
  (let [args (repeatedly arity gensym)]
    `(fn [~@args] (~'js* ~template ~@args))))

(defn parse-decl [decl]
  (map (fn [[args & body]] (cons (cons `do body) args))
    (when decl (if (vector? (first decl)) (list decl) decl))))

(defn parse-clause [clause]
  (map #(list 'quote %) (if (seq? clause) clause (list clause))))

(defn global [s]
  (keyword
    (let [n (namespace s)]
      (case n "cljs.core" "clojure.core" n))
    (name s)))

(defn free [env ast]
  (case (:op ast)
    (:local)
    (if (contains? env (:name ast))
      {} {(:name ast) (:form ast)})

    (:let :loop)
    (let [{:keys [bindings body]} ast]
      ((fn rec [env bindings]
         (if-some [[binding & bindings] bindings]
           (merge (free env (:init binding))
                  (rec (assoc env (:name binding) (:form binding)) bindings))
           (free env body))) env (seq bindings)))

    (:invoke)
    (->> (cons (:fn ast) (:args ast))
         (map (partial free env))
         (apply merge))

    (->> (map ast (:children ast))
         (map (partial free env))
         (apply merge))))

(defn free-variables [local methods]
  (->> methods
    (map (fn [{:keys [params body]}]
           (free (into {} (map (juxt :name :form))
                   (if local (cons local params) params)) body)))
    (apply merge)))

(defn with-local [env sym]
  (let [l (::local env)
        i (get (::index env) l 0)]
    (-> env
      (update ::index assoc l (inc i))
      (update :locals assoc sym {::pub [l i]}))))

(defn qualify-sym "If ast node is `:var`, update :form to be a fully qualified symbol" [env ast]
  (if (and (= :var (:op ast)) (not (-> ast :env :def-var)))
    (assoc ast :form (case (env/peer-language env)
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
        edef?           (fn [ast] (or (some? (::node (:meta ast)))
                                    (some? (::node (:meta (:info ast))))))
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
                          (swap! refered-lexical assoc form (gensym (name form))))
        record-edef!    (fn [{:keys [form] :as ast}]
                          (if (dynamic? ast)
                            (swap! refered-evars assoc form #_(ana/var-name ast) (gensym (name form)))
                            (record-lexical! ast)))
        env             (update env :locals update-vals #(if (map? %) (assoc % ::provided? true) {::provided? true}))
        rewrite-ast     (fn [ast]
                          (cond
                            (edef? ast)    (do (record-edef! ast)
                                               (cond (dynamic? ast)    (qualify-sym env ast)
                                                     (namespaced? ast) (update ast :form safe-let-name)
                                                     :else             ast))
                            (lexical? ast) (do (record-lexical! ast) ast)
                            :else          (qualify-sym env ast)))
        form            (case (env/peer-language env)
                          :clj  (-> (ana/analyze-clj (env/clj-env env) form)
                                  (ana/walk-clj rewrite-ast)
                                  (ana/emit-clj))
                          :cljs (-> (ana/analyze-cljs env form)
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

(tests
  (def -env {:ns (symbol (str *ns*)) ::peers-config {::local :clj, ::remote :clj} ::local true})
  (closure -env '(fn* ([a b c] [a b c rec])))
  := [_ ['rec]]
  )

(defn source-map
  ([env debug-info]
   (let [file (if (:js-globals env) (:file (:meta (:ns env)))
                  (:file (meta (find-ns (:ns env)))))]
     (if (some? file)
       (merge {:file file} debug-info)
       (or debug-info {})))))

(defn analyze-global [env sym]
  (case (namespace sym)
    "js" (ir/eval sym)
    (let [sym (if-some [var (env/resolve-var env sym)]
                (env/var-name var)
                (case (env/peer-language env)
                  :clj  (if (and (namespace sym) (maybe-class-from-string (namespace sym))) ; static field
                          sym
                          (throw (ex-info (str "Unable to resolve symbol: " sym) (source-map env (meta sym)))))
                  :cljs (let [sym (symbol (or (namespace sym) (name (:name (:ns env)))) (name sym))]
                          (log/debug "cljs var: " sym " is undeclared, passing through." (source-map env (meta sym)))
                          sym)))]
      (assoc (ir/global (global sym))
        ::dbg/meta (source-map env (meta sym))))))

(defn pure-res [local & remotes]
  {:local local
   :remote (vec remotes)})

(defn bind-res [rx f & args]
  (let [ry (apply f (:local rx) args)]
    {:local (:local ry)
     :remote (into (:remote rx) (:remote ry))}))

(defmacro let-res [bs & body]
  (if-some [[s i & bs] (seq bs)]
    `(bind-res ~i (fn [~s] (let-res ~bs ~@body)))
    `(pure-res (do ~@body))))

(defn map-res [f & rs]
  (bind-res
    (reduce
      (fn [rv rx]
        (let-res [v rv, x rx]
          (conj v x))) (pure-res []) rs)
    (fn [args] (pure-res (apply f args)))))

(declare analyze-form visit-node)

(defn resolve-node [env sym]
  (if-let [var (env/resolve-var env sym)]
    (if (env/is-node var)
      (visit-node env (env/var-name var) (::node (env/var-meta var)))
      (throw (ex-info (str "Not a reactive var: " (env/var-name var)) {})))
    (throw (ex-info (str "Unable to resolve symbol: " sym) {}))))

(defn analyze-symbol [env sym]
  (if (contains? (:locals env) sym)
    (if-some [[p i] (::pub (get (:locals env) sym))]
      (let [s (assoc (ir/sub (- (get (::index env) p) i))
                ::dbg/name sym
                ::dbg/scope :lexical
                ::dbg/meta (meta sym))]
        (if (= p (::local env))
          (pure-res s)
          (pure-res
            (assoc (ir/input [])
              ::dbg/name  sym
              ::dbg/scope :dynamic
              ::dbg/meta  (meta sym))
            (ir/output s))))
      (pure-res (assoc (ir/global (keyword sym))
                  ::dbg/meta (source-map env (meta sym)))))
    (if-some [sym (env/resolve-runtime env sym)]
      (pure-res (analyze-global env sym))
      (if-some [v (env/resolve-var env sym)]
        (if (env/is-node v)
          (pure-res (assoc (ir/node (resolve-node env sym))
                      ::dbg/name  (env/var-name v)
                      ::dbg/scope :dynamic))
          (throw (ex-info "Can't take value of macro." {:symbol (env/var-name v)})))
        (pure-res (analyze-global env (env/resolve-alias env sym))) ; pass through
        ))))

(defn analyze-apply [env sexp]
  (->> sexp
    (map (partial analyze-form env))
    (apply map-res ir/apply)))

(defn causal [stmt expr]
  (ir/apply (ir/literal {}) stmt expr))

(defn causal-publish [init inst]
  (ir/pub init (causal (ir/sub 1) inst)))

(defn analyze-binding [env bs f & args]
  ((fn rec [env bs ns]
     (if-some [[node form & bs] bs]
       (let-res [init (analyze-form env form)
                 inst (rec (update env ::index update (::local env) (fnil inc 0)) bs (conj ns node))]
         (causal-publish init inst))
       (reduce-kv
         (fn [res i n]
           (map-res
             (fn [inst]
               (ir/bind (resolve-node env n)
                 (- (count ns) i) inst)) res))
         (apply f env args) ns))) env (seq bs) []))

(defn dotted-symbol? "Return true if `sym` starts with `.`, excluding `..`."
  [sym]
  (some? (re-find #"^\.[^.]" (name sym))))

(defn desugar-host [env form]
  (case (env/peer-language env)
    :cljs (if (and (seq? form) (dotted-symbol? (first form)))
            (let [[op target & args] form]
              (list* '. target (symbol (subs (name op) 1)) args))
            form)
    :clj (let [env (env/clj-env env)]
           (binding [*ns* (the-ns (:ns env))] ; tools.analyzer use *ns* to resolve implicit imports (java.lang ...)
             (if (and (seq? form) (qualified-symbol? (first form)))
               (clj-env/with-env {:namespaces {(:ns env) (env/resolve-ns (:ns env))}}
                 (clj/desugar-host-expr form env))
               (clj/desugar-host-expr form env))))))

(defn toggle [env form debug-info]
  (let [res (analyze-form (update env ::local not) form)]
    {:local  (ir/input (:remote res))
     :remote [(merge (ir/output (:local res)) debug-info)]}))

(defn- ?add-default-throw [clauses env switch-val-sym]
  (cond-> clauses
    (even? (count clauses))
    (conj `(throw (new ~(if (= (env/peer-language env) :clj) 'IllegalArgumentException 'js/Error)
                    (str "No matching clause: " ~switch-val-sym))))))

(defn- ->case-clause [op]
  (fn [[test expr]] `(::closure ~expr {::dbg/type :case-clause ::dbg/args [~test] ::dbg/meta ~(meta op)})))

(defn- case-vals->v [partition symbols]
  (reduce merge {}
    (map (fn [p s] (zipmap (parse-clause (first p)) (repeat s)))
      partition symbols)))

(defn- rewrite-letfn*-bindings-for-clj-analysis [bindings] `(letfn* ~bindings ~(vec (take-nth 2 bindings))))
(tests
  (rewrite-letfn*-bindings-for-clj-analysis '[f (fn f [] (g)) g (fn g [] (f))])
  := '(letfn* [f (fn f [] (g)) g (fn g [] (f))] [f g]))

(defn- split-letfn*-clj&photon-analysis [[_ bindings & body]]
  `(let [~(vec (take-nth 2 bindings)) (::letfn ~bindings)] ~@body))
(tests
  (split-letfn*-clj&photon-analysis '(letfn* [f (fn f [] (g)) g (fn g [] (f))] (new Foo)))
  := '(clojure.core/let [[f g] (::letfn [f (fn f [] (g)) g (fn g [] (f))])] (new Foo)))


(defn analyze-sexpr [env [op & args :as form]]
  (case op
    (ns ns* deftype* defrecord* var)
    (throw (ex-info "Unsupported operation." {:op op :args args}))

    (let*)
    ((fn rec [env bs]
       (if-some [[s i & bs] bs]
         (map-res causal-publish (analyze-form env i) (rec (with-local env s) bs))
         (analyze-sexpr env (cons `do (next args)))))
     env (seq (first args)))

    (do)
    (if-some [[x & xs] args]
      ((fn rec [x xs]
         (let [r (analyze-form env x)]
           (if-some [[y & ys] xs]
             (map-res causal r (rec y ys))
             r))) x xs)
      (pure-res (ir/literal nil)))

    (if)
    (let [[test then else] args]
      (analyze-form env `(case ~test (nil false) ~else ~then)))

    (case clojure.core/case cljs.core/case)
    (analyze-form env
      (let [clauses        (vec (next args))
            switch-val-sym (gensym)
            clauses        (?add-default-throw clauses env switch-val-sym)
            partition      (partition-all 2 (pop clauses))
            symbols        (repeatedly gensym)
            default        (list ::closure (peek clauses) {::dbg/type :case-default})]
        `(new
           (let [~switch-val-sym ~(first args)
                 ~@(interleave symbols (map (->case-clause op) partition))]
             (~(case-vals->v partition symbols) ~switch-val-sym ~default)))))

    (quote)
    (pure-res (ir/literal (first args)))

    ;; TODO
    (js*)
    (if-some [[f & args] args]
      (->> args
        (map (partial analyze-form env))
        (apply map-res
          (partial ir/apply
            (assoc (ir/eval `(js-call ~f ~(count args)))
              ::dbg/action :js-call))))
      (throw (ex-info "Wrong number of arguments - js*" {})))

    (fn*)
    (let [sym         (and (symbol? (first args)) (first args))
          [form refs] (closure env form)]
      (->> refs
        (map (partial analyze-form env))
        (apply map-res
          (partial ir/apply
            (merge
              (ir/eval form)
              {::ir/ns (or (some-> env :ns :name) 'user)}
              {::dbg/action :fn-call
               ::dbg/params ['...]}                         ; TODO add parameters to debug info
              (when sym {::dbg/name sym}))))))

    (letfn*)
    (analyze-sexpr env (split-letfn*-clj&photon-analysis form))

    (::letfn)
    (let [[form refs] (closure env (rewrite-letfn*-bindings-for-clj-analysis (first args)))]
      (->> refs
        (map (partial analyze-form env))
        (apply map-res
          (partial ir/apply
            (assoc (ir/eval form)
              ::dbg/type :letfn)))))

    (set!)
    (let [[form refs] (closure env form)]
      (->> refs
        (map (partial analyze-form env))
        (apply map-res
          (partial ir/apply
            (assoc (ir/eval form)
              ::dbg/type :set!)))))

    (new)                                                   ; argument binding + monadic join
    (if-some [[f & args] args]
      (if-some [ctor (when (symbol? f)                      ; detect clj/cljs class
                       (when-not (contains? (:locals env) f)
                         (env/resolve-runtime env f)))]

                                        ; clj/cljs class interop
        (if (or (= :cljs (env/peer-language env))
              (instance? CljClass (env/resolve-var env f)))
          (->> args
            (map (partial analyze-form env))
            (apply map-res
              (partial ir/apply
                (ir/eval `(ctor-call ~ctor ~(count args))))))
          (throw (ex-info (str "Cannot call `new` on " f) (source-map env (meta form)))))

                                        ; boot signal (monadic join), with arguments passed as dynamic scope
        (let [sym (if (or (contains? (:locals env) f)
                        (not (symbol? f))) ; e.g. (new (identity Foo))
                    f
                    (if-some [var (env/resolve-var env f)]
                      (if (env/is-node var)
                        (env/var-name var)
                        (throw (ex-info (str "Not a reactive def: " f) (source-map env (meta form)))))
                      (throw (ex-info (str "Unable to resolve symbol: " f) (source-map env (meta form))))))]
          (let-res [ctor (analyze-form env sym)
                    inst (analyze-binding (update env ::index update (::local env) (fnil inc 0))
                           (list* `%arity (count args) (interleave arg-sym args))
                           (fn [_]
                             (pure-res
                               (ir/variable
                                 (ir/sub (+ 2 (count args))))
                               ir/source)))]
            (causal-publish ctor inst))))
      (throw (ex-info "Wrong number of arguments - new" {})))

    (.)
    (let [dot    (cljs/build-dot-form [(first args) (second args) (nnext args)])
          target (:target dot)
          target (cond (class? target)  (CljClass. (:target dot))
                       (symbol? target) (env/resolve-var env (:target dot))
                       :else            target)]
      (->> (if (instance? CljClass target)
             (:args dot)
             (cons (:target dot) (:args dot)))
        (map (partial analyze-form env))
        (apply map-res
          (partial ir/apply
            (assoc (ir/eval
                     (if (instance? CljClass target)
                       `(static-call ~(env/var-name target) ~(:method dot) ~(count (:args dot)))
                       (case (:dot-action dot)
                         ::cljs/call `(method-call ~(:method dot) ~(count (:args dot)))
                         ::cljs/access `(field-access ~(:field dot)))))
              ::dbg/meta (meta form)
              ::dbg/action (if (instance? CljClass target)
                             :static-call
                             (case (:dot-action dot)
                               ::cljs/call   :call
                               ::cljs/access :field-access))
              ::dbg/target (if (satisfies? env/IVar target) (env/var-name target) target)
              ::dbg/method (or (:method dot) (:field dot))
              ::dbg/args   (:args dot))))))

    (throw)
    (analyze-form env `(r/fail ~(first args)))

    (try)
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
                          (throw (ex-info "Invalid try block - unrecognized clause." {})))))
                (throw (ex-info "Invalid try block - finally must be in final position." {}))))
            [[] [] nil] args)
          body `(::closure (do ~@forms) {::dbg/type :try ::dbg/meta ~(meta op)})]
      (analyze-form env
        `(new ~(reduce
                 (fn [r f] `(r/latest-first ~r (::closure ~f {::dbg/type :finally})))
                 (case catches
                   [] body
                   `(r/bind r/recover
                      (some-fn
                        ~@(map (fn [[c s & body]]
                                 (let [f `(partial (::inject exception) (::closure
                                                                    (let [~s (dbg/unwrap exception)]
                                                                      (binding [hyperfiddle.electric/trace exception]
                                                                        ~@body))
                                                                    {::dbg/type :catch, ::dbg/args [~c ~s]}))]
                                   (case c
                                     (:default Throwable)
                                     `(r/clause ~f)
                                     `(r/clause ~f ~c)))) catches))
                      ~body)) finally))))

    (loop*)
    (let [[bindings & body] args, bs (vec (take-nth 2 bindings)), vs (vec (take-nth 2 (rest bindings)))]
      (analyze-form env `(binding [rec (::closure (let [~@(interleave bs arg-sym)] ~@body))]
                           (new rec ~@vs))))

    (recur)
    (analyze-form env `(new rec ~@args))

    (def)
    (let [[sym v] args]
      (if (some->> sym (env/resolve-var env) env/is-node)
        (throw (ex-info "Cannot `def` a reactive var" {:var sym}))
        (analyze-form env (if (= :cljs (env/peer-language env))
                            (let [ns (-> env :ns :name)]
                              (swap! cljs.env/*compiler* assoc-in [::cljs/namespaces ns :defs sym]
                                {:name sym})
                              ;; FIXME don't let when `set!` is fixed
                              `(let [v# ~v] (set! ~(symbol (str ns) (str sym)) v#)))
                            `((fn [x#] (def ~sym x#)) ~v)))))

    (::lift)
    (map-res ir/lift
      (analyze-form env (first args)))

    (::closure)
    (let [[form debug-info] args
          res               (analyze-form env form)]
      (pure-res
        (merge (ir/constant (:local res)) debug-info)
        (ir/target (:remote res))))

    (::client)
    (let [[form debug-info] args]
      ((if (::local env) analyze-form #(toggle %1 %2 {::dbg/meta (source-map env debug-info)})) env form))

    (::server)
    (let [[form debug-info] args]
      ((if (::local env) #(toggle %1 %2 {::dbg/meta (source-map env debug-info)}) analyze-form) env form))

    (::inject)
    (pure-res (ir/inject (resolve-node env (first args))))

    (if (symbol? op)
      (let [n (name op)
            e (dec (count n))]
        (if (and (not= ".." n) (= \. (nth n e))) ; "Class.", leave cljs.core$macros/.. alone
          (analyze-sexpr env (with-meta `(new ~(symbol (namespace op) (subs n 0 e)) ~@args) (meta form)))
          (if (contains? (:locals env) op)
            (analyze-apply env form)
            (if-some [sym (env/resolve-runtime env op)]
              (case sym
                (clojure.core/unquote-splicing cljs.core/unquote-splicing) (toggle env (first args) {::dbg/meta (meta form) ::dbg/type :toggle})
                (analyze-apply env form))
              (if-some [v (env/resolve-var env op)]
                (case (env/var-name v)
                  (clojure.core/binding cljs.core/binding)
                  (analyze-binding env (first args) analyze-sexpr (cons `do (next args)))

                  (if (env/is-node v)
                    (analyze-apply env form)
                    (cond (instance? CljVar v) ; "manual" macroexpansion: call the var as a function, passing it &form and the appropriate &env
                          (analyze-form env
                            (binding [*env* env
                                      *ns* (find-ns (if (:js-globals env) (:name (:ns env)) (:ns env)))]
                              (apply (env/get-var v) form (if (:js-globals env) env (:locals env)) args)))

                          (instance? CljsVar v) ;; TODO GG: is this case possible? A cljs macro var without a corresponding clj macro var.
                          (throw (ex-info "Failed to resolve macro expander" {:name (env/var-name v)}))

                          :else (throw (ex-info "Invalid call" {:form form})))))
                (let [desugared-form (desugar-host env form)]
                  (if (= form desugared-form)
                    ;; Nothing got desugared, this is not host interop, meaning `op` var wasn't found.
                    (analyze-apply env form) ; pass through
                    (analyze-form env desugared-form))))))))
      (analyze-apply env form))))

(defn analyze-map [env form]
  (analyze-form env (if-let [m (meta form)]
                      (list `with-meta (cons `hash-map (sequence cat form)) m)
                      (cons `hash-map (sequence cat form)))))

(defn analyze-set [env form]
  (analyze-form env (cons `hash-set form)))

(defn analyze-vector [env form]
  (analyze-form env (cons `vector form)))

(defn analyze-js [_env _form]
  (assert nil "Not implemented : #js"))

(defn analyze-form [env form]
  (if-let [analyze
           (or
            (and (symbol? form) analyze-symbol)
            (and (seq? form) (seq form) analyze-sexpr)
            (and (map? form) analyze-map)
            (and (vector? form) analyze-vector)
            (and (set? form) analyze-set)
            (and (instance? JSValue form) analyze-js))]
    (try (analyze env form)
         (catch clojure.lang.ExceptionInfo ex
           ;; GG: Report problematic form in context.
           ;;     Build a stack of forms as the call stack unwinds, from failed form to top-level form.
           ;;     Push current form into ex-data, then rethrow.
           ;; TODO revisit
           (throw (ex-info (ex-message ex) (update (ex-data ex) :in #(if (< (count %) 1)
                                                                       (conj (or % []) form)
                                                                       %)) (ex-cause ex))))
         (catch Throwable t ; base case
           (throw (ex-info "Failed to analyse form" {:in [form]} t))))
    (pure-res (ir/literal form))))

(defn await-cljs-namespace
  "Await for a cljs namespace to be compiled, then return it. Blocking."
  ;; CLJS namespaces are compiled in parallel. While shadow track dependencies
  ;; properly, running long task during macroexpansion phase can introduce
  ;; races. Is this a bug in Shadow?
  [sym-ns]
  (or (cljs/get-namespace sym-ns)
    (let [task (->> (m/ap (get-in (m/?< (m/watch cljs.env/*compiler*)) [::cljs/namespaces sym-ns]))
                 (m/eduction (remove nil?) (take 1))
                 (m/reduce {} nil))]
      (m/? (m/timeout task 60000)))))

(let [nodes (l/local)]
  (defn visit-node
    "Given a symbol naming a reactive var, and the form to analyze. 
     Analyze form in the namespace context of sym."
    [env sym form]                      ;; TODO detect cycles
    (let [l (::local env)]
      (if-some [node (-> (.get nodes) (get l) (get sym))]
        (:slot node)
        (let [sym-ns (symbol (namespace sym))
              inst (case form
                     ::unbound nil
                     (if (:js-globals env)
                       (if-let [ns (await-cljs-namespace sym-ns)] ; get cljs namespace to analyze node in.
                         (analyze-form (-> (assoc env :ns ns :locals {}) (dissoc ::index)) form) ; clean up locals as form is not in lexical scope of the caller.
                         (throw (ex-info "Can't analyze reactive var reference: no such ClojureScript namespace. This var should be defined in a cljs or cljc file." {:symbol sym, :namespace sym-ns})))
                       (analyze-form (-> (assoc env :ns sym-ns :locals {}) (dissoc ::index)) form)))
              slot (-> (.get nodes) (get l) count)]
          (.set nodes (update (.get nodes) l update sym assoc :inst inst :slot slot :rank
                              (+ slot (-> (.get nodes) (get (not l)) count)))) slot))))

  (defn analyze [env form]
    (let [[res nodes]
          (l/with-local nodes {}
            (let [env (assoc (env/normalize-env env) ::local true)]
              ;; ClojureScript compiler binds *ns* to the current ns: where macroexpand is happening.
              ;; When syntax-quoted, the `*ns*` form expands to `(read-string #=(find-ns current-ns-sym))` - because of print-dup.
              ;; If the current ns is a cljs only namespace, it won’t exist on the JVM, and `find-ns` will fail to resolve with this error:
              ;; Error: ExceptionInInitializerError, Caused by RuntimeException: Can't resolve find-ns
              ;; An `ExceptionInInitializerError` means the Clojure compiler fails to resolve a var during the static initialization
              ;; of a class.
              ;; clojure.tools.logging macros includes *ns* in their macroexpansion.
              ;; From now on in the code, we carry ns info info in `env`, and we reset `*ns*` to the current clj namespace
              ;; to *ns* is an available namespace. The Electric compiler never rely on *ns*, only on `env`.
              (binding [*ns* (find-ns 'hyperfiddle.electric.impl.compiler)]
                (analyze-form env form))))
          nodes (->> nodes
                     (mapcat (fn [[peer nodes]]
                               (map (partial merge {:peer peer}) (vals nodes))))
                     (filter :inst)
                     (sort-by :rank))]
      (mapv (fn [p]
              ((fn rec [nodes]
                 (if-some [[{:keys [peer slot inst]} & nodes] (seq nodes)]
                   (if (= p peer)
                     (ir/pub (:local inst)
                       (ir/bind slot 1 (rec nodes)))
                     (ir/do (:remote inst) (rec nodes)))
                   (if p
                     (:local res)
                     (ir/do (:remote res) ir/nop))))
               nodes)) [true false]))))

(tests

  (analyze {} '5) :=
  [(ir/literal 5)
   (ir/do [] ir/nop)]

  (analyze {} '(+ 2 3)) :=
  [(ir/apply (assoc (ir/global :clojure.core/+) ::dbg/meta {})
     (ir/literal 2) (ir/literal 3))
   (ir/do [] ir/nop)]

  (analyze {} '(let [a 1] (+ a 2))) :=
  [(ir/pub (ir/literal 1)
     (ir/apply (ir/literal {})
       (ir/sub 1)
       (ir/apply (assoc (ir/global :clojure.core/+) ::dbg/meta {})
         (assoc (ir/sub 1) ::dbg/name 'a ::dbg/scope :lexical ::dbg/meta nil)
         (ir/literal 2))))
   (ir/do [] ir/nop)]

  (analyze '{a nil} 'a) :=
  [(assoc (ir/global :a) ::dbg/meta {})
   (ir/do [] ir/nop)]

  (doto (def Ctor) (alter-meta! assoc :macro true ::node ::unbound))
  (analyze {} '(Ctor.)) :=
  [(ir/pub (assoc (ir/node 0)
             ::dbg/name `Ctor
             ::dbg/scope :dynamic)
     (ir/apply (ir/literal {})
       (ir/sub 1)
       (ir/pub (ir/literal 0)
         (ir/apply (ir/literal {})
           (ir/sub 1)
           (ir/bind 1 1
             (ir/variable (ir/sub 2)))))))
   (ir/do [ir/source] ir/nop)]

  (analyze {} '~@:foo) :=
  [(ir/input [])
   (ir/do [(assoc (ir/output (ir/literal :foo))
             ::dbg/type :toggle
             ::dbg/meta nil)] ir/nop)]

  (analyze {} '(::closure :foo)) :=
  [(ir/constant (ir/literal :foo))
   (ir/do [(ir/target [])] ir/nop)]

  (analyze {} '(do :a :b)) :=
  [(ir/apply (ir/literal {})
     (ir/literal :a) (ir/literal :b))
   (ir/do [] ir/nop)]

  (analyze {} '(case 1 2 3 (4 5) ~@6 7)) :=
  [(ir/pub (ir/pub (ir/literal 1)
             (ir/apply (ir/literal {})
               (ir/sub 1)
               (ir/pub (assoc (ir/constant (ir/literal 3))
                         ::dbg/type :case-clause
                         ::dbg/args [2]
                         ::dbg/meta nil)
                 (ir/apply (ir/literal {})
                   (ir/sub 1)
                   (ir/pub (assoc (ir/constant (ir/input []))
                             ::dbg/type :case-clause
                             ::dbg/args ['(4 5)]
                             ::dbg/meta nil)
                     (ir/apply (ir/literal {})
                       (ir/sub 1)
                       (ir/apply (ir/apply (assoc (ir/global :clojure.core/hash-map) ::dbg/meta {})
                                   (ir/literal 2)
                                   (assoc (ir/sub 2)
                                     ::dbg/name _
                                     ::dbg/scope :lexical
                                     ::dbg/meta nil)
                                   (ir/literal 4)
                                   (assoc (ir/sub 1)
                                     ::dbg/name _
                                     ::dbg/scope :lexical
                                     ::dbg/meta nil)
                                   (ir/literal 5)
                                   (assoc (ir/sub 1)
                                     ::dbg/name _
                                     ::dbg/scope :lexical
                                     ::dbg/meta nil))
                         (assoc (ir/sub 3)
                           ::dbg/name _
                           ::dbg/scope :lexical
                           ::dbg/meta nil)
                         (assoc (ir/constant (ir/literal 7))
                           ::dbg/type :case-default))))))))
     (ir/apply (ir/literal {})
       (ir/sub 1)
       (ir/pub (ir/literal 0)
         (ir/apply (ir/literal {})
           (ir/sub 1)
           (ir/bind 0 1 (ir/variable (ir/sub 2)))))))
   (ir/do [(ir/target [])
           (ir/target [(assoc (ir/output (ir/literal 6))
                         ::dbg/meta nil
                         ::dbg/type :toggle)])
           (ir/target [])
           ir/source]
          ir/nop)]

  (analyze {} '(case 1 2 3 (4 5) ~@6)) :=
  [(ir/pub (ir/pub (ir/literal 1)
             (ir/apply (ir/literal {})
               (ir/sub 1)
               (ir/pub (assoc (ir/constant (ir/literal 3))
                         ::dbg/type :case-clause
                         ::dbg/args [2]
                         ::dbg/meta nil)
                 (ir/apply (ir/literal {})
                   (ir/sub 1)
                   (ir/pub (assoc (ir/constant (ir/input []))
                             ::dbg/type :case-clause
                             ::dbg/args ['(4 5)]
                             ::dbg/meta nil)
                     (ir/apply (ir/literal {})
                       (ir/sub 1)
                       (ir/apply (ir/apply (assoc (ir/global :clojure.core/hash-map)
                                             ::dbg/meta {})
                                   (ir/literal 2)
                                   (assoc (ir/sub 2)
                                     ::dbg/name _
                                     ::dbg/scope :lexical
                                     ::dbg/meta nil)
                                   (ir/literal 4)
                                   (assoc (ir/sub 1)
                                     ::dbg/name _
                                     ::dbg/scope :lexical
                                     ::dbg/meta nil)
                                   (ir/literal 5)
                                   (assoc (ir/sub 1)
                                     ::dbg/name _
                                     ::dbg/scope :lexical
                                     ::dbg/meta nil))
                         (assoc (ir/sub 3)
                           ::dbg/name _
                           ::dbg/scope :lexical
                           ::dbg/meta nil)
                         (assoc (ir/constant (ir/apply (assoc (ir/global :hyperfiddle.electric.impl.runtime/fail) ::dbg/meta {})
                                               (ir/apply (ir/eval '(hyperfiddle.electric.impl.compiler/ctor-call java.lang.IllegalArgumentException 1))
                                                 (ir/apply (assoc (ir/global :clojure.core/str) ::dbg/meta {})
                                                   (ir/literal "No matching clause: ")
                                                   (assoc (ir/sub 3)
                                                     ::dbg/name _
                                                     ::dbg/scope :lexical
                                                     ::dbg/meta nil)))))
                           ::dbg/type :case-default))))))))
     (ir/apply (ir/literal {})
       (ir/sub 1)
       (ir/pub (ir/literal 0)
         (ir/apply (ir/literal {})
           (ir/sub 1)
           (ir/bind 0 1 (ir/variable (ir/sub 2)))))))

   (ir/do [(ir/target [])
           (ir/target [(assoc (ir/output (ir/literal 6))
                         ::dbg/meta nil
                         ::dbg/type :toggle)])
           (ir/target [])
           ir/source]
          ir/nop)]

  (doto (def foo) (alter-meta! assoc :macro true ::node nil))
  (doto (def bar) (alter-meta! assoc :macro true ::node 'foo))
  (analyze {} 'bar) :=
  [(ir/pub (ir/literal nil)
     (ir/bind 0 1
       (ir/pub (assoc (ir/node 0)
                 ::dbg/name `foo
                 ::dbg/scope :dynamic)
         (ir/bind 1 1
           (assoc (ir/node 1)
             ::dbg/name `bar
             ::dbg/scope :dynamic)))))
   (ir/do [] (ir/do [] (ir/do [] ir/nop)))]

  (analyze {} '(::inject foo)) :=
  [(ir/pub (ir/literal nil)
     (ir/bind 0 1
       (ir/inject 0)))
   (ir/do [] (ir/do [] ir/nop))]

  (analyze {} '(let [a 1] (new ((::inject foo) (::closure ~@(new (::closure ~@foo))) (::closure a)))))
  :=
  [(ir/pub (ir/literal nil)
     (ir/bind 0 1
       (ir/pub (ir/literal 1)
         (ir/apply (ir/literal {})
           (ir/sub 1)
           (ir/pub (ir/apply (ir/inject 0)
                     (ir/constant
                       (ir/input [(ir/target
                                    [(assoc (ir/output
                                              (assoc (ir/node 0)
                                                ::dbg/name  `foo
                                                ::dbg/scope :dynamic))
                                       ::dbg/meta nil
                                       ::dbg/type :toggle)])
                                  ir/source]))
                     (ir/constant
                       (assoc (ir/sub 1)
                         ::dbg/name  'a
                         ::dbg/scope :lexical
                         ::dbg/meta  nil)))
             (ir/apply (ir/literal {})
               (ir/sub 1)
               (ir/pub (ir/literal 0)
                 (ir/apply (ir/literal {})
                   (ir/sub 1)
                   (ir/bind 1 1
                     (ir/variable (ir/sub 2)))))))))))

   (ir/do [] (ir/do [(ir/target
                       [(assoc (ir/output
                                 (ir/pub (ir/constant (ir/input []))
                                   (ir/apply (ir/literal {})
                                     (ir/sub 1)
                                     (ir/pub (ir/literal 0)
                                       (ir/apply (ir/literal {})
                                         (ir/sub 1)
                                         (ir/bind 0 1
                                           (ir/variable (ir/sub 2))))))))
                          ::dbg/meta nil
                          ::dbg/type :toggle)])
                     (ir/target [])
                     ir/source] ir/nop))]

  (doto (def baz) (alter-meta! assoc :macro true ::node '(::closure ~@foo)))
  (analyze {} '(let [a 1] (new ((::inject foo) (::closure ~@(new baz)) (::closure a))))) :=
  [(ir/pub (ir/literal nil)
     (ir/bind 0 1
       (ir/do [(ir/target
                 [(assoc (ir/output
                           (assoc (ir/node 0)
                             ::dbg/name  `foo
                             ::dbg/scope :dynamic))
                    ::dbg/meta nil
                    ::dbg/type :toggle)])]
              (ir/pub (ir/literal 1)
                (ir/apply (ir/literal {})
                  (ir/sub 1)
                  (ir/pub (ir/apply (ir/inject 0)
                            (ir/constant (ir/input [ir/source]))
                            (ir/constant (assoc (ir/sub 1)
                                           ::dbg/name  'a
                                           ::dbg/scope :lexical
                                           ::dbg/meta  nil)))
                    (ir/apply (ir/literal {})
                      (ir/sub 1)
                      (ir/pub (ir/literal 0)
                        (ir/apply (ir/literal {})
                          (ir/sub 1)
                          (ir/bind 1 1 (ir/variable (ir/sub 2))))))))))))
   (ir/do [] (ir/pub (ir/constant (ir/input []))
               (ir/bind 0 1
                 (ir/do [(ir/target
                           [(assoc (ir/output
                                     (ir/pub (assoc (ir/node 0)
                                               ::dbg/name  `baz
                                               ::dbg/scope :dynamic)
                                       (ir/apply (ir/literal {})
                                         (ir/sub 1)
                                         (ir/pub (ir/literal 0)
                                           (ir/apply (ir/literal {})
                                             (ir/sub 1)
                                             (ir/bind 1 1
                                               (ir/variable (ir/sub 2))))))))
                              ::dbg/meta nil
                              ::dbg/type :toggle)])
                         (ir/target [])
                         ir/source] ir/nop))))]

  (analyze {} '(try 1 (finally 2 3 4))) :=
  [(ir/pub (ir/apply (assoc (ir/global ::r/latest-first)
                       ::dbg/meta {})
             (ir/apply (assoc (ir/global ::r/latest-first)
                         ::dbg/meta {})
               (ir/apply {::ir/op ::ir/global
                          ::ir/name ::r/latest-first
                          ::dbg/meta {}}
                 (assoc (ir/constant (ir/literal 1))
                   ::dbg/type :try
                   ::dbg/meta nil)
                 (assoc (ir/constant (ir/literal 2))
                   ::dbg/type :finally))
               (assoc (ir/constant (ir/literal 3))
                 ::dbg/type :finally))
             (assoc (ir/constant (ir/literal 4))
               ::dbg/type :finally))
     (ir/apply (ir/literal {})
       (ir/sub 1)
       (ir/pub (ir/literal 0)
         (ir/apply (ir/literal {})
           (ir/sub 1)
           (ir/bind 0 1
             (ir/variable (ir/sub 2)))))))
   (ir/do [(ir/target [])
           (ir/target [])
           (ir/target [])
           (ir/target [])
           ir/source] ir/nop)]

  (analyze {} '(try 1 (catch Exception e 2) (finally 3))) :=
  [(ir/pub (ir/literal nil)
     (ir/bind 1 1
       (ir/pub (ir/apply (assoc (ir/global ::r/latest-first)
                           ::dbg/meta {})
                 (ir/apply (assoc (ir/global ::r/bind)
                             ::dbg/meta {})
                   (assoc (ir/global ::r/recover)
                     ::dbg/meta {})
                   (ir/apply (assoc (ir/global :clojure.core/some-fn)
                               ::dbg/meta {})
                     (ir/apply (assoc (ir/global ::r/clause)
                                 ::dbg/meta {})
                       (ir/apply (assoc (ir/global :clojure.core/partial)
                                   ::dbg/meta {})
                         (ir/inject 0)
                         (assoc (ir/constant (ir/pub (ir/apply (assoc (ir/global ::dbg/unwrap)
                                                                 ::dbg/meta {})
                                                       (assoc (ir/node 0)
                                                         ::dbg/name `exception
                                                         ::dbg/scope :dynamic))
                                               (ir/apply (ir/literal {})
                                                 (ir/sub 1)
                                                 (ir/pub (assoc (ir/node 0)
                                                           ::dbg/name `exception
                                                           ::dbg/scope :dynamic)
                                                   (ir/apply (ir/literal {})
                                                     (ir/sub 1)
                                                     (ir/bind 1 1
                                                       (ir/literal 2)))))))
                           ::dbg/type :catch
                           ::dbg/args '[Exception e]))
                       (assoc (ir/global :java.lang.Exception)
                         ::dbg/meta {})))
                   (assoc (ir/constant (ir/literal 1))
                     ::dbg/type :try
                     ::dbg/meta nil))
                 (assoc (ir/constant (ir/literal 3))
                   ::dbg/type :finally))
         (ir/apply (ir/literal {})
           (ir/sub 1)
           (ir/pub (ir/literal 0)
             (ir/apply (ir/literal {})
               (ir/sub 1)
               (ir/bind 2 1
                 (ir/variable (ir/sub 2)))))))))
   (ir/do [] (ir/do [(ir/target [])
                     (ir/target [])
                     (ir/target [])
                     ir/source] ir/nop))]

  (analyze {} '(::server (::client 1))) :=
  [(ir/input [(assoc (ir/output (ir/literal 1)) ::dbg/meta {})])
   (ir/do [(assoc (ir/output (ir/input [])) ::dbg/meta {})] ir/nop)])

(tests "literals"
  (analyze {} {:a 1}) :=
  [(ir/apply (assoc (ir/global :clojure.core/hash-map) ::dbg/meta {})
     (ir/literal :a)
     (ir/literal 1))
   (ir/do [] ir/nop)]
  (analyze {} ^{:b 2} {:a 1}) :=
  [(ir/apply (assoc (ir/global :clojure.core/with-meta) ::dbg/meta {})
     (ir/apply (assoc (ir/global :clojure.core/hash-map) ::dbg/meta {})
       (ir/literal :a)
       (ir/literal 1))
     (ir/apply (assoc (ir/global :clojure.core/hash-map) ::dbg/meta {})
       (ir/literal :b)
       (ir/literal 2)))
   (ir/do [] ir/nop)])

(comment
  ;; GG: dev only - cljs compiler error and warning messages are vague.
  ;;    uncomment and adapt to get more info.
  (defmethod cljs/error-message :undeclared-var
    [warning-type info]
    (throw (ex-info "Manual interception - undeclared var" {:info info}))
    (str (if (:macro-present? info)
           "Can't take value of macro "
           "Use of undeclared Var ")
         (:prefix info) "/" (:suffix info)))
  )

(tests "method access"
  (analyze {} '(. Math abs -1))  :=
  [(ir/apply (assoc (ir/eval '(hyperfiddle.electric.impl.compiler/static-call java.lang.Math abs 1))
               ::dbg/action :static-call
               ::dbg/target 'java.lang.Math
               ::dbg/method 'abs
               ::dbg/args   '(-1)
               ::dbg/meta {:line _ :column 16})
     (ir/literal -1))
   (ir/do [] ir/nop)]

  (analyze {} '(Math/abs -1))  :=
  [(ir/apply (assoc (ir/eval '(hyperfiddle.electric.impl.compiler/static-call java.lang.Math abs 1))
               ::dbg/action :static-call
               ::dbg/target 'java.lang.Math
               ::dbg/method 'abs
               ::dbg/args   '(-1)
               ::dbg/meta {:line _ :column 16})
     (ir/literal -1))
   (ir/do [] ir/nop)])

(tests
  "undeclared vars"
  (try (analyze {} 'Foo)
       (catch clojure.lang.ExceptionInfo e
         (ex-message e) := "Unable to resolve symbol: Foo"))

  (try (analyze {} '(Foo.))
       (catch clojure.lang.ExceptionInfo e
         (ex-message e) := "Unable to resolve symbol: Foo")))

(tests
  "Incorrect usage of `new`"
  (declare not-a-class)                 ; available at runtime but not a class
  (try (analyze {} '(not-a-class.))
       (catch clojure.lang.ExceptionInfo e
         (ex-message e) := "Cannot call `new` on not-a-class"))

  (def ^:macro not-a-reactive-def)    ; not available at runtime and not a p/def
  (try (analyze {} '(not-a-reactive-def.))
       (catch clojure.lang.ExceptionInfo e
         (ex-message e) := "Not a reactive def: not-a-reactive-def")))

(tests "loop/recur"
  ;; just check if it compiles
  (analyze {} '(loop [x 1] (recur (inc x)))) := _)

(tests "clojure def for electric def is a mistake"
  (def ^{::node ::unbound, :macro true} wont-work)
  (try (analyze {} '(def wont-work 12))
       (catch clojure.lang.ExceptionInfo e
         (ex-message e) := "Cannot `def` a reactive var")))
