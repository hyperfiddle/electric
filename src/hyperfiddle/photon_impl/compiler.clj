(ns hyperfiddle.photon-impl.compiler
  (:require [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.env :as env]
            [cljs.analyzer :as cljs]
            [cljs.util]
            [hyperfiddle.photon-impl.local :as l]
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.photon.debug :as dbg]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as str]
            [hyperfiddle.photon-impl.analyzer :as ana]
            [hyperfiddle.logger :as log]
            [clojure.tools.analyzer.jvm.utils :refer [maybe-class-from-string]])
  (:import cljs.tagged_literals.JSValue
           (clojure.lang Var)))

(def ^{:dynamic true, :doc "Bound to Photon compiler env when macroexpension is managed by Photon."} *env*)

;; %1, %2 … %n p/def generator.
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

(defmacro method-call [method arity]
  (let [args (repeatedly arity gensym)]
    `(fn [inst# ~@args] (. inst# ~method ~@args))))

(defmacro field-access [field]
  `(fn [inst#] (. inst# ~(symbol (str "-" (name field))))))

(defmacro js-call [template arity]
  (let [args (repeatedly arity gensym)]
    `(fn [~@args] (~'js* ~template ~@args))))

(defmacro fn-call [f args] `(fn [~@args] ~f))

(defn normalize-env [env]
  (let [peers-config (or (::peers-config env) (if (:js-globals env) {::local :cljs, ::remote :cljs} {::local :clj, ::remote :clj}))]
    (if (:js-globals env)
      (assoc env ::peers-config peers-config)
      {:ns            (ns-name *ns*)
       :locals        (dissoc env ::peers-config)
       ::peers-config peers-config})))

(defn clj-env [?cljs-env]
  (if (:js-globals ?cljs-env)
    (-> ?cljs-env (dissoc :js-globals) (assoc :ns (:name (:ns ?cljs-env))))
    ?cljs-env))

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

;;; Resolving

(defn resolve-ns "Builds a description of a namespace. Returns nil if no namespace can be found for the given symbol."
  [ns-sym]
  (when-let [ns (find-ns ns-sym)]
    {:mappings (merge (ns-map ns) {'in-ns #'clojure.core/in-ns
                                   'ns    #'clojure.core/ns})
     :aliases  (reduce-kv (fn [a k v] (assoc a k (ns-name v)))
                          {} (ns-aliases ns))
     :interns  (ns-interns ns)
     :ns       (ns-name ns)}))

(defn resolve-sym "Expand a qualified symbol to its fully qualified form, according to ns aliases."
  [env sym]
  (if (simple-symbol? sym) sym
    (let [ns (if (:js-globals env) (:name (:ns env)) (:ns env))]
      (as-> sym $
        (symbol (namespace $)) ; extract namespace part of sym
        (get-in (resolve-ns ns) [:aliases $] $) ; expand to fully qualified form
        (symbol (str $) (name sym))
        (with-meta $ (meta sym))
        ))))

;; GG: Abstraction on resolved vars.
;;     A symbol may resolve to:
;;     - nil
;;     - a cljs var description (a map)
;;     - a cljs macro var description (a map)
;;     - a clojure var (clojure.lang.Var)
;;     - a JVM Class
(defprotocol IVar
  (get-var [this])
  (var-name [this])
  (var-meta [this])
  (is-macro [this])
  (is-node [this]))

(deftype CljVar [var]
  IVar
  (get-var  [_this] var)
  (var-name [_this] (let [m (meta var)] (symbol (name (ns-name (:ns m))) (name (:name m)))))
  (var-meta [_this] (meta var))
  (is-macro [_this] (.isMacro ^Var var))
  (is-node  [this] (contains? (var-meta this) ::node)))

(deftype CljsVar [var]
  IVar
  (get-var  [_this] var)
  (var-name [_this] (:name var))
  (var-meta [_this] (:meta var))
  (is-macro [this] (:macro (or (var-meta this) var)))
  (is-node  [this] (contains? (or (var-meta this) var) ::node)))

(deftype CljClass [klass]
  IVar
  (get-var  [_this] klass)
  (var-name [_this] (symbol (.getName ^Class klass)))
  (var-meta [_this] nil)
  (is-macro [_this] false)
  (is-node  [_this] false))

(defmacro no-warn 
  "Localy disable a set of cljs compiler warning.
  Usage: `(no-warn #{:undeclared-ns} (cljs/resolve env sym))`"
  [disabled-warnings & body]
  `(binding [cljs/*cljs-warnings* (reduce (fn [r# k#] (assoc r# k# false)) cljs/*cljs-warnings* ~disabled-warnings)]
     ~@body))

(defn resolve-cljs ; GG: adapted from cljs.analyzer.api/resolve, return an IVar.
  "Given an analysis environment resolve a var. Analogous to
   clojure.core/resolve"
  [env sym]
  {:pre [(map? env) (symbol? sym)]}
  (try
    (binding [cljs/*private-var-access-nowarn* true]
      (let [klass (clojure.lang.Compiler/maybeResolveIn (the-ns (:name (:ns env))) sym)]
        (if (class? klass)
          (CljClass. klass)
          (CljsVar. (no-warn #{:undeclared-ns} (cljs/resolve-var env sym (cljs/confirm-var-exists-throw)))))))
    (catch Exception _e
      (when-some [v (cljs/resolve-macro-var env sym)]
        (CljsVar. v)))))

(defn peer-language [env]
  (case (::local env)
    (true nil) (get-in env [::peers-config ::local])
    false      (get-in env [::peers-config ::remote])))

(defn is-cljs-file? [env]
  (and (:js-globals env)
    (some-> env :ns :meta :file (str/ends-with? ".cljs"))))

(defn resolve-var
  "Resolve a clojure or clojurescript var, given these rules:
   If the resolved clojurescript var is a macro var, return the corresponding clojure var.
   Return an IVar or nil."
  [env sym]
  (case (peer-language env)
    :clj  (if (is-cljs-file? env)
            (throw (ex-info "Cannot resolve a Clojure expression from a cljs namespace. Use a .cljc file." {:file (:file (:meta (:ns env)))}))
            (let [env      (clj-env env)
                  ns       (resolve-ns (:ns env)) ; current ns
                  resolved (if (simple-symbol? sym)
                             (get-in ns [:mappings sym]) ; resolve in current ns
                             (as-> sym $
                               (symbol (namespace $)) ; extract namespace part of sym
                               (get-in ns [:aliases $] $) ; expand to fully qualified form
                               (get-in (resolve-ns $) [:interns (symbol (name sym))]) ; resolve declared var in target ns
                               ))]
              (if (some? resolved)
                (cond (var? resolved)   (CljVar. resolved)
                      (class? resolved) (CljClass. resolved)
                      :else             (throw (ex-info "Symbol resolved to an unknow type" {:symbol sym
                                                                                             :type   (type resolved)
                                                                                             :value  resolved})))
                ;; java.lang is implicit so not listed in ns form or env
                (when-some [resolved (clojure.lang.Compiler/maybeResolveIn (the-ns (:ns env)) sym)]
                  (CljClass. resolved)))))
    :cljs (let [var (resolve-cljs env sym)] ; resolve cljs var decription (a map)
            (if (and var (= :local (:op (get-var var)))) ; if lexical binding
              nil
              (if-let [expander (cljs/get-expander (if (some? var) (var-name var) sym) env)] ; find corresponding clojure var
                (CljVar. expander)
                var)))))

(defn resolve-runtime
  "Returns the fully qualified symbol of the var resolved by given symbol at runtime, or nil if:
  - it cannot be resolved,
  - it doesn't exist at runtime (is a macro),
  - sym is a special form."
  [env sym]
  (letfn [(runtime-symbol [var] (when (some? var)
                                  (when-not (or (is-macro var) (is-node var))
                                    (with-meta (var-name var) (meta sym)))))]
    (case (peer-language env)
      :clj  (runtime-symbol (resolve-var env sym))
      :cljs (if-let [v (resolve-var env sym)]
              (cond (instance? CljsVar v)  (runtime-symbol v)
                    (instance? CljClass v) (runtime-symbol v)
                    ;; GG: if sym resolves to a clojure var, look up for the cljs-specific version.
                    ;;     Why: some vars exist in two versions (e.g. cljs.core/inc)
                    ;;          - the cljs version is a function (available at runtime),
                    ;;          - the clj version is a macro expending to optimized code (compile-time only).
                    (instance? CljVar v)   (runtime-symbol (resolve-cljs env sym)))
              ;; GG: corner case: there is no var for cljs.core/unquote-splicing.
              (when (= 'cljs.core/unquote-splicing (:name (cljs/resolve-var env sym)))
                'cljs.core/unquote-splicing)))))

(defn resolve-runtime-throwing [env form]
  (when-not (symbol? form) (throw (ex-info "Symbol expected." {:form form})))
  (doto (resolve-runtime env form)
    (-> (when-not (throw (ex-info "Unable to resolve symbol." {:form form}))))))

(defn vars [env forms]
  (let [resolved (into {} (map (comp (juxt global identity)
                                     (partial resolve-runtime-throwing (normalize-env env))))
                       forms)]
    `(fn ~'global-var-resolver [not-found# ident#]
       (case ident#
         ~@(mapcat identity resolved)
         not-found#))))

;;; ----

(defn with-local [env sym]
  (let [l (::local env)
        i (get (::index env) l 0)]
    (-> env
      (update ::index assoc l (inc i))
      (update :locals assoc sym {::pub [l i]}))))

(defn qualify-sym "If ast node is `:var`, update :form to be a fully qualified symbol" [env ast]
  (if (= :var (:op ast))
    (assoc ast :form (case (peer-language env)
                       :clj  (symbol (str (:ns (:meta ast))) (str (:name (:meta ast))))
                       :cljs (:name (:info ast))))
    ast))

(defn provided-bindings [env form]
  (let [bindings         (volatile! {})
        provided?        (fn [ast-info] (or (::provided? ast-info) (some? (::node (:meta ast-info)))))
        record-provided! (fn [{:keys [form] :as ast}]
                           (if (contains? @bindings form)
                             (assoc ast :form (get @bindings form))
                             (let [safe-name (gensym (name form))]
                               (vswap! bindings assoc form safe-name)
                               (assoc ast :form safe-name))))
        env              (update env :locals update-vals #(if (map? %) (assoc %  ::provided? true) {::provided? true}))]
    [(case (peer-language env)
       :clj  (-> (ana/analyze-clj (clj-env env) form)
               (ana/walk-clj (fn [ast] (if (provided? ast)
                                         (record-provided! ast)
                                         (qualify-sym env ast))))
               (ana/emit-clj))
       :cljs (-> (ana/analyze-cljs env form)
               (ana/walk-cljs (fn [ast] (if (provided? (:info ast))
                                          (record-provided! ast)
                                          (qualify-sym env ast))))
               ana/emit-cljs))
     @bindings]))

(defn source-map
  ([env debug-info]
   (let [file (if (:js-globals env) (:file (:meta (:ns env)))
                  (:file (meta (find-ns (:ns env)))))]
     (if (some? file)
       (merge {:file file} debug-info)
       (or debug-info {})))))

(defn analyze-global [env sym]
  (case (namespace sym)
    "js" [:eval sym]
    (let [sym (if-some [var (resolve-var env sym)]
                (var-name var)
                (case (peer-language env)
                  :clj  (if (and (namespace sym) (maybe-class-from-string (namespace sym))) ; static field
                          sym
                          (throw (ex-info (str "Unable to resolve symbol: " sym) (source-map env (meta sym)))))
                  :cljs (do (log/warn "Use of undeclared var" (symbol (or (namespace sym) (name (:name (:ns env)))) (name sym))
                              (source-map env (meta sym)))
                            sym)))]
      [:global (global sym) (source-map env (meta sym))])))

(defn conj-res
  ([x] x)
  ([x y]
   (conj (into (pop x) (pop y))
     (conj (peek x) (peek y))))
  ([x y & zs]
   (conj (into (pop x) (mapcat pop) (cons y zs))
     (transduce (map peek) conj (peek x) (cons y zs)))))

(defn void [cont]
  (fn rec [insts]
    (if-some [[inst & insts] (seq insts)]
      (conj inst (rec insts)) cont)))

(declare analyze-form visit-node)

(defn resolve-node [env sym]
  (if-let [var (resolve-var env sym)]
    (if (is-node var)
      (visit-node env (var-name var) (::node (var-meta var)))
      (throw (ex-info (str "Not a reactive var: " (var-name var)) {})))
    (throw (ex-info (str "Unable to resolve symbol: " sym) {}))))

(defn analyze-symbol [env sym]
  (if (contains? (:locals env) sym)
    (if-some [[p i] (::pub (get (:locals env) sym))]
      (let [s [:sub (- (get (::index env) p) i) (assoc (meta sym) ::dbg/name sym ::dbg/scope :lexical)]]
        (if (= p (::local env))
          [s] [[:output nil s] [:input (assoc (meta sym) ::dbg/name sym ::dbg/scope :dynamic)]]))
      [[:global (keyword sym) (source-map env (meta sym))]])
    (if-some [sym (resolve-runtime env sym)]
      [(analyze-global env sym)]
      (if-some [v (resolve-var env sym)]
        (if (is-node v)
          [[:node (visit-node env (var-name v) (::node (var-meta v))) {::dbg/name (var-name v) ::dbg/scope :dynamic}]]
          (throw (ex-info "Can't take value of macro." {:symbol (var-name v)})))
        [(analyze-global env (resolve-sym env sym))] ; pass through
        ))))

(defn analyze-apply [env form]
  (transduce
   (map (partial analyze-form env))
   conj-res [[:apply]] form))

(defn analyze-binding [env bs f & args]
  (let [l (::local env)]
    ((fn rec [env bs ns]
       (if-some [[node form & bs] bs]
         (-> [[:pub]]
           (conj-res (analyze-form env form))
           (conj-res (conj-res [[:apply [:literal {}] [:sub 1]]]
                       (rec (update env ::index update l (fnil inc 0)) bs (conj ns node)))))
         (reduce-kv
           (fn [res i n]
             (conj-res [[:bind (resolve-node env n) (- (count ns) i)]] res))
           (apply f env args) ns))) env (seq bs) [])))

(defn desugar-host [env form]
  (case (peer-language env)
    :cljs (if (and (seq? form) (cljs/dotted-symbol? (first form)))
            (let [[op target & args] form]
              (list* '. target (symbol (subs (name op) 1)) args))
            form)
    :clj (let [env (clj-env env)]
           (binding [*ns* (the-ns (:ns env))] ; tools.analyzer use *ns* to resolve implicit imports (java.lang …)
             (if (and (seq? form) (qualified-symbol? (first form)))
               (env/with-env {:namespaces {(:ns env) (resolve-ns (:ns env))}}
                 (clj/desugar-host-expr form env))
               (clj/desugar-host-expr form env))))))

(defn toggle [env form debug-info]
  (let [res (analyze-form (update env ::local not) form)]
    [[:output debug-info (peek res)] ((void [:input]) (pop res))]))

(defn analyze-sexpr [env [op & args :as form]]
  (case op
    (letfn* loop* recur set! ns ns* deftype* defrecord* var)
    (throw (ex-info "Unsupported operation." {:op op :args args}))

    (let*)
    ((fn rec [env bs]
       (if-some [[s i & bs] bs]
         (-> [[:pub]]
           (conj-res (analyze-form env i))
           (conj-res (conj-res [[:apply [:literal {}] [:sub 1]]]
                       (rec (with-local env s) bs))))
         (analyze-sexpr env (cons `do (next args)))))
     env (seq (first args)))

    (do)
    (if-some [[x & xs] args]
      (case xs
        nil (analyze-form env x)
        (-> [[:apply [:literal {}]]]
          (conj-res (analyze-form env x))
          (conj-res (analyze-sexpr env (cons `do xs)))))
      [[:literal nil]])

    (if)
    (let [[test then else] args]
      (analyze-form env `(case ~test (nil false) ~else ~then)))

    (case clojure.core/case cljs.core/case)
    (analyze-form env
      (let [clauses   (vec (next args))
            total?    (odd? (count clauses))
            partition (partition-all 2 (if total? (pop clauses) clauses))
            symbols   (repeatedly gensym)]
        (->> (when total? (list ::closure (peek clauses) {::dbg/type :case-default}))
          (list
            (reduce merge {}
              (map (fn [p s] (zipmap (parse-clause (first p)) (repeat s)))
                partition symbols))
            (first args))
          (list `let (vec (interleave symbols
                            (map (fn [p] (list ::closure (second p) (merge {::dbg/type :case-clause, ::dbg/args [(first p)]}
                                                                      (meta op))))
                              partition))))
          (list `new))))

    (quote)
    [[:literal (first args)]]

    (def)
    (transduce (map (comp vector (partial resolve-node env)))
      conj-res [[:def]] args)

    (js*)
    (if-some [[f & args] args]
      (transduce (map (partial analyze-form env)) conj-res
        [[:eval `(js-call ~f ~(count args)) {::dbg/action :js-call}]] args)
      (throw (ex-info "Wrong number of arguments - js*" {})))

    (fn*)
    (let [sym              (and (symbol? (first args)) (first args))
          [form bindings] (provided-bindings env form)]
      (transduce (map (partial analyze-form env)) conj-res
        [[:apply [:eval `(fn-call ~form ~(vals bindings)) (merge {::dbg/action :fn-call, ::dbg/params ['…]} ; TODO add parameters to debug info
                                                            (when sym {::dbg/name sym}))]]] (keys bindings)))

    (new)                                                   ; argument binding + monadic join
    (if-some [[f & args] args]
      (if-some [ctor (when (symbol? f)                      ; detect clj/cljs class
                       (when-not (contains? (:locals env) f)
                         (resolve-runtime env f)))]

        ; clj/cljs class interop
        (if (or (= :cljs (peer-language env))
              (instance? CljClass (resolve-var env f)))
          (transduce (map (partial analyze-form env)) conj-res
            [[:apply [:eval `(ctor-call ~ctor ~(count args))]]] args)
          (throw (ex-info (str "Cannot call `new` on " f) (source-map env (meta form)))))

        ; boot signal (monadic join), with arguments passed as dynamic scope
        (let [sym (if (or (contains? (:locals env) f)
                          (not (symbol? f))) ; e.g. (new (identity Foo))
                    f
                    (if-some [var (resolve-var env f)]
                      (if (is-node var)
                        (var-name var)
                        (throw (ex-info (str "Not a reactive def: " f) (source-map env (meta form)))))
                      (throw (ex-info (str "Unable to resolve symbol: " f) (source-map env (meta form))))))]
          (analyze-binding env (interleave arg-sym (cons sym args)) (fn [_] [[:source] [:variable [:sub (inc (count args))]]]))))
      (throw (ex-info "Wrong number of arguments - new" {})))

    (.)
    (let [dot    (cljs/build-dot-form [(first args) (second args) (nnext args)])
          target (:target dot)
          target (cond (class? target)  (CljClass. (:target dot))
                       (symbol? target) (resolve-var env (:target dot))
                       :else            target)]
      (transduce (map (partial analyze-form env)) conj-res
        [[:apply [:eval (if (instance? CljClass target)
                          `(static-call ~(var-name target) ~(:method dot) ~(count (:args dot)))
                          (case (:dot-action dot)
                            ::cljs/call   `(method-call ~(:method dot) ~(count (:args dot)))
                            ::cljs/access `(field-access ~(:field dot))))
                  (merge (meta form)
                    {::dbg/action (if (instance? CljClass target)
                                    :static-call
                                    (case (:dot-action dot)
                                      ::cljs/call   :call
                                      ::cljs/access :field-access))
                     ::dbg/target (if (satisfies? IVar target) (var-name target) target)
                     ::dbg/method (or (:method dot) (:field dot))
                     ::dbg/args   (:args dot)})]]]
        (if (instance? CljClass target)
          (:args dot)
          (cons (:target dot) (:args dot)))))

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
          body `(::closure (do ~@forms) ~(merge {::dbg/type :try} (meta op)))]
      (analyze-form env
        `(new ~(reduce
                 (fn [r f] `(r/latest-first ~r (::closure ~f {::dbg/type :finally})))
                 (case catches
                   [] body
                   `(r/bind r/recover
                      (some-fn
                        ~@(map (fn [[c s & body]]
                                 (let [f `(partial (def exception) (::closure
                                                                    (let [~s (dbg/unwrap exception)]
                                                                      (binding [hyperfiddle.photon/trace exception]
                                                                        ~@body))
                                                                    {::dbg/type :catch, ::dbg/args [~c ~s]}))]
                                   (case c
                                     (:default Throwable)
                                     `(r/clause ~f)
                                     `(r/clause ~f ~c)))) catches))
                      ~body)) finally))))

    (::lift)
    (conj-res [[:lift]] (analyze-form env (first args)))

    (::closure)
    (let [[form debug-info] args
          res               (analyze-form env form)]
      [[:target ((void [:nop]) (pop res))] [:constant (peek res) debug-info]])

    (::client)
    (let [[form debug-info] args]
      ((if (::local env) analyze-form #(toggle %1 %2 (source-map env debug-info))) env form))

    (::server)
    (let [[form debug-info] args]
      ((if (::local env) #(toggle %1 %2 (source-map env debug-info)) analyze-form) env form))

    (if (symbol? op)
      (let [n (name op)
            e (dec (count n))]
        (if (and (not= ".." n) (= \. (nth n e))) ; "Class.", leave cljs.core$macros/.. alone
          (analyze-sexpr env (with-meta `(new ~(symbol (namespace op) (subs n 0 e)) ~@args) (meta form)))
          (if (contains? (:locals env) op)
            (analyze-apply env form)
            (if-some [sym (resolve-runtime env op)]
              (case sym
                (clojure.core/unquote-splicing cljs.core/unquote-splicing) (toggle env (first args) (merge (meta form) {::dbg/type :toggle}))
                (analyze-apply env form))
              (if-some [v (resolve-var env op)]
                (case (var-name v)
                  (clojure.core/binding cljs.core/binding)
                  (analyze-binding env (first args) analyze-sexpr (cons `do (next args)))

                  (if (is-node v)
                    (analyze-apply env form)
                    (cond (instance? CljVar v) ; "manual" macroexpansion: call the var as a function, passing it &form and the appropriate &env
                          (analyze-form env
                            (binding [*env* env]
                              (apply (get-var v) form (if (:js-globals env) env (:locals env)) args)))

                          (instance? CljsVar v) ;; TODO GG: is this case possible? A cljs macro var without a corresponding clj macro var.
                          (throw (ex-info "Failed to resolve macro expander" {:name (var-name v)}))

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
           (throw (ex-info (ex-message ex) (update (ex-data ex) :in #(if (< (count %) 10)
                                                                       (conj (or % []) form)
                                                                       %)) (ex-cause ex))))
         (catch Throwable t ; base case
           (throw (ex-info "Failed to analyse form" {:in [form]} t))))
    [[:literal form]]))

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
                       (if-let [ns (cljs/get-namespace sym-ns)] ; get cljs namespace to analyze node in.
                         (analyze-form (-> (assoc env :ns ns :locals {}) (dissoc ::index)) form) ; clean up locals as form is not in lexical scope of the caller.
                         (throw (ex-info "Can't analyze reactive var reference: no such ClojureScript namespace. This var should be defined in a cljs or cljc file." {:symbol sym, :namespace sym-ns})))
                       (analyze-form (-> (assoc env :ns sym-ns :locals {}) (dissoc ::index)) form)))
              slot (-> (.get nodes) (get l) count)]
          (.set nodes (update (.get nodes) l update sym assoc :inst inst :slot slot :rank
                              (+ slot (-> (.get nodes) (get (not l)) count)))) slot))))
  
  (defn analyze [env form]
    (let [[inst nodes]
          (l/with-local nodes {}
            (-> env
                (normalize-env)
                (assoc ::local true)
                (analyze-form form)))
          nodes (->> nodes
                     (mapcat (fn [[peer nodes]]
                               (map (partial merge {:peer peer}) (vals nodes))))
                     (filter :inst)
                     (sort-by :rank))]
      (mapv (fn [p]
              ((fn rec [nodes]
                 (if-some [[{:keys [peer slot inst]} & nodes] (seq nodes)]
                   (if (= p peer)
                     [:pub (peek inst) [:bind slot 1 (rec nodes)]]
                     ((void (rec nodes)) (pop inst)))
                   (if p (peek inst) ((void [:nop]) (pop inst)))))
               nodes)) [true false]))))

(tests

  (analyze {} '5) :=
  [[:literal 5] [:nop]]

  (analyze {} '(+ 2 3)) :=
  [[:apply [:global :clojure.core/+ {}] [:literal 2] [:literal 3]]
   [:nop]]

  (analyze {} '(let [a 1] (+ a 2))) :=
  [[:pub [:literal 1]
    [:apply [:literal {}] [:sub 1]
     [:apply [:global :clojure.core/+ {}]
      [:sub 1 {::dbg/name 'a ::dbg/scope :lexical}] [:literal 2]]]]
   [:nop]]

  (analyze '{a nil} 'a) :=
  [[:global :a {}]
   [:nop]]

  (doto (def Ctor) (alter-meta! assoc :macro true ::node ::unbound))
  (analyze {} '(Ctor.)) :=
  [[:pub [:node 0 {::dbg/name `Ctor, ::dbg/scope :dynamic}]
    [:apply [:literal {}] [:sub 1]
     [:bind 1 1 [:variable [:sub 1]]]]]
   [:source [:nop]]]

  (analyze {} '~@:foo) :=
  [[:input]
   [:output {::dbg/type :toggle} [:literal :foo] [:nop]]]  ; TODO debug info would be better placed last

  (analyze {} '(::closure :foo)) :=
  [[:constant [:literal :foo] nil]
   [:target [:nop] [:nop]]]

  (analyze {} '(do :a :b)) :=
  [[:apply [:literal {}]
    [:literal :a] [:literal :b]]
   [:nop]]

  (analyze {} '(case 1 2 3 (4 5) ~@6 7)) :=
  [[:pub
     [:pub
      [:constant [:literal 3] {::dbg/type :case-clause, ::dbg/args [2]}]
      [:apply [:literal {}] [:sub 1]
       [:pub
        [:constant [:input] {::dbg/type :case-clause, ::dbg/args ['(4 5)]}]
        [:apply [:literal {}] [:sub 1]
         [:apply
          [:apply [:global :clojure.core/hash-map nil]
           [:literal 2] [:sub 2 {::dbg/name _  ::dbg/scope :lexical}]
           [:literal 4] [:sub 1 {::dbg/name ?x ::dbg/scope :lexical}]
           [:literal 5] [:sub 1 {::dbg/name ?x ::dbg/scope :lexical}]]
          [:literal 1]
          [:constant [:literal 7] {::dbg/type :case-default}]]]]]]
     [:apply [:literal {}] [:sub 1]
      [:bind 0 1 [:variable [:sub 1]]]]]
    [:target [:nop]
     [:target [:output {::dbg/type :toggle} [:literal 6] [:nop]]
      [:target [:nop]
       [:source [:nop]]]]]]

  (analyze {} '(case 1 2 3 (4 5) ~@6)) :=
  [[:pub
    [:pub
     [:constant [:literal 3] {::dbg/type :case-clause, ::dbg/args [2]}]
     [:apply [:literal {}] [:sub 1]
      [:pub
       [:constant [:input] {::dbg/type :case-clause, ::dbg/args ['(4 5)]}]
       [:apply [:literal {}] [:sub 1]
        [:apply
         [:apply [:global :clojure.core/hash-map nil]
          [:literal 2] [:sub 2 {::dbg/name _  ::dbg/scope :lexical}]
          [:literal 4] [:sub 1 {::dbg/name ?x ::dbg/scope :lexical}]
          [:literal 5] [:sub 1 {::dbg/name ?x ::dbg/scope :lexical}]]
         [:literal 1]
         [:literal nil]]]]]]
    [:apply [:literal {}] [:sub 1]
     [:bind 0 1 [:variable [:sub 1]]]]]
   [:target [:nop]
    [:target [:output {::dbg/type :toggle} [:literal 6] [:nop]]
     [:source [:nop]]]]]

  (doto (def foo) (alter-meta! assoc :macro true ::node nil))
  (doto (def bar) (alter-meta! assoc :macro true ::node 'foo))
  (analyze {} 'bar) :=
  [[:pub [:literal nil] [:bind 0 1 [:pub [:node 0 {::dbg/name `foo ::dbg/scope :dynamic}]
                                    [:bind 1 1 [:node 1 {::dbg/name `bar ::dbg/scope :dynamic}]]]]]
   [:nop]]

  (analyze {} '(def foo)) :=
  [[:pub [:literal nil] [:bind 0 1 [:def 0]]]
   [:nop]]

  (analyze {} '(let [a 1] (new ((def foo) (::closure ~@(new (::closure ~@foo))) (::closure a)))))
  :=
  [[:pub [:literal nil]
    [:bind 0 1
     [:pub [:literal 1]
      [:apply [:literal {}] [:sub 1]
       [:pub
        [:apply [:def 0]
         [:constant [:target [:output {::dbg/type :toggle} [:node 0 {::dbg/name `foo, ::dbg/scope :dynamic}] [:nop]] [:source [:input]]] nil]
         [:constant [:sub 1 {::dbg/name 'a, ::dbg/scope :lexical}] nil]]
        [:apply [:literal {}] [:sub 1]
         [:bind 1 1 [:variable [:sub 1]]]]]]]]]
   [:target
    [:output {::dbg/type :toggle}
     [:pub [:constant [:input] nil]
      [:apply [:literal {}] [:sub 1]
       [:bind 0 1 [:variable [:sub 1]]]]] [:nop]]
    [:target [:nop] [:source [:nop]]]]]

  (doto (def baz) (alter-meta! assoc :macro true ::node '(::closure ~@foo)))
  (analyze {} '(let [a 1] (new ((def foo) (::closure ~@(new baz)) (::closure a))))) :=
  [[:pub [:literal nil]
    [:bind 0 1
     [:target [:output {::dbg/type :toggle} [:node 0 {::dbg/name `foo ::dbg/scope :dynamic}] [:nop]]
      [:pub [:literal 1]
       [:apply [:literal {}] [:sub 1]
        [:pub [:apply [:def 0] [:constant [:source [:input]] nil] [:constant [:sub 1 {::dbg/name 'a ::dbg/scope :lexical}] nil]]
         [:apply [:literal {}] [:sub 1]
          [:bind 1 1 [:variable [:sub 1]]]]]]]]]]
   [:pub [:constant [:input] nil]
    [:bind 0 1
     [:target [:output {::dbg/type :toggle} [:pub [:node 0 {::dbg/name `baz, ::dbg/scope :dynamic}]
                                             [:apply [:literal {}] [:sub 1]
                                              [:bind 1 1 [:variable [:sub 1]]]]] [:nop]]
      [:target [:nop] [:source [:nop]]]]]]]

  (analyze {} '(try 1 (finally 2 3 4))) :=
  [[:pub [:apply
          [:global ::r/latest-first {}]
          [:apply [:global ::r/latest-first {}]
           [:apply [:global ::r/latest-first {}]
            [:constant [:literal 1] {::dbg/type :try}] [:constant [:literal 2] {::dbg/type :finally}]]
           [:constant [:literal 3] {::dbg/type :finally}]]
          [:constant [:literal 4] {::dbg/type :finally}]]
    [:apply [:literal {}] [:sub 1]
     [:bind 0 1 [:variable [:sub 1]]]]]
   [:target [:nop]
    [:target [:nop]
     [:target [:nop]
      [:target [:nop]
       [:source [:nop]]]]]]]

  (analyze {} '(try 1 (catch Exception e 2) (finally 3))) :=
  [[:pub [:literal nil] [:bind 1 1
                         [:pub [:apply [:global ::r/latest-first {}]
                                [:apply [:global ::r/bind {}]
                                 [:global ::r/recover {}]
                                 [:apply [:global :clojure.core/some-fn {}]
                                  [:apply [:global ::r/clause {}]
                                   [:apply [:global :clojure.core/partial {}]
                                    [:def 0] [:constant [:pub [:apply [:global :hyperfiddle.photon.debug/unwrap {}]
                                                               [:node 0 {::dbg/name `exception ::dbg/scope :dynamic}]]
                                                         [:apply [:literal {}] [:sub 1]
                                                          [:pub [:node 0 {::dbg/name `exception ::dbg/scope :dynamic}]
                                                           [:apply [:literal {}] [:sub 1]
                                                            [:bind 1 1 [:literal 2]]]]]]
                                              {::dbg/type :catch, ::dbg/args '[Exception e]}]]
                                   [:global :java.lang.Exception {}]]]
                                 [:constant [:literal 1] {::dbg/type :try}]]
                                [:constant [:literal 3] {::dbg/type :finally}]]
                          [:apply [:literal {}] [:sub 1]
                           [:bind 2 1 [:variable [:sub 1]]]]]]]
   [:target [:nop]
    [:target [:nop]
     [:target [:nop]
      [:source [:nop]]]]]]
  )


(tests "literals"
  (analyze {} {:a 1}) := [[:apply [:global :clojure.core/hash-map {}] [:literal :a] [:literal 1]] [:nop]]
  (analyze {} ^{:b 2} {:a 1}) := [[:apply
                                   [:global :clojure.core/with-meta {}]
                                   [:apply [:global :clojure.core/hash-map {}] [:literal :a] [:literal 1]]
                                   [:apply [:global :clojure.core/hash-map {}] [:literal :b] [:literal 2]]]
                                  [:nop]]
  )

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

(tests
  "Var resolution"
  ;; resolve on the default peer (local) with default compiler env (clj)
  (-> (normalize-env {})
    (resolve-var 'inc)
    get-var)
  :=  #'clojure.core/inc

  (-> (normalize-env {})
    (resolve-var 'java.lang.Integer)
    get-var)
  := java.lang.Integer

  (require 'cljs.core)
  ;; resolve on the local (cljs) peer
  (binding [cljs.env/*compiler* (cljs.env/default-compiler-env)]
    (-> (cljs.analyzer/empty-env)
      (assoc ::peers-config {::local :cljs ::remote :clj})
      (normalize-env)
      (assoc ::local true)            ; set current peer to local
      (resolve-var 'inc)
      get-var))
  := (resolve 'cljs.core/inc)


  ;; resolve on the remote (clj) peer
  (binding [cljs.env/*compiler* (cljs.env/default-compiler-env)]
    (-> (cljs.analyzer/empty-env)
      (assoc-in [:ns :name] (ns-name *ns*))  ; default cljs ns is cljs.user, which is not a thing.
      (assoc ::peers-config {::local :cljs ::remote :clj})
      (normalize-env)
      (assoc ::local false)               ; set current peer to remote
      (resolve-var 'inc)
      get-var))
  := #'clojure.core/inc
  )

(tests "method access"
  (analyze {} '(. Math abs -1))  :=
  [[:apply [:eval '(hyperfiddle.photon-impl.compiler/static-call java.lang.Math abs 1) {:line _,
                                                                                        :column 16,
                                                                                        :hyperfiddle.photon.debug/action :static-call,
                                                                                        :hyperfiddle.photon.debug/target 'java.lang.Math,
                                                                                        :hyperfiddle.photon.debug/method 'abs,
                                                                                        :hyperfiddle.photon.debug/args '(-1)}]
    [:literal -1]] [:nop]]

  (analyze {} '(Math/abs -1))  :=
  [[:apply [:eval '(hyperfiddle.photon-impl.compiler/static-call java.lang.Math abs 1) {:line _,
                                                                                        :column 16,
                                                                                        :hyperfiddle.photon.debug/action :static-call,
                                                                                        :hyperfiddle.photon.debug/target 'java.lang.Math,
                                                                                        :hyperfiddle.photon.debug/method 'abs,
                                                                                        :hyperfiddle.photon.debug/args '(-1)}]
    [:literal -1]] [:nop]]
  )

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

