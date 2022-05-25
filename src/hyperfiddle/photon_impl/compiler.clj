(ns hyperfiddle.photon-impl.compiler
  (:require [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.env :as env]
            [cljs.analyzer :as cljs]
            [cljs.util]
            [hyperfiddle.photon-impl.local :as l]
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.rcf :refer [tests]])
  (:import cljs.tagged_literals.JSValue
           (clojure.lang Var)))

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

(defmacro method-call [method arity]
  (let [args (repeatedly arity gensym)]
    `(fn [inst# ~@args] (. inst# ~method ~@args))))

(defmacro field-access [field]
  `(fn [inst#] (. inst# ~(symbol (str "-" (name field))))))

(defmacro js-call [template arity]
  (let [args (repeatedly arity gensym)]
    `(fn [~@args] (~'js* ~template ~@args))))

(defn normalize-env [env]
  (if (:js-globals env)
    env {:ns (ns-name *ns*) :locals env}))

(defn parse-decl [decl]
  (map (fn [[args & body]] (cons (cons `do body) args))
    (when decl (if (vector? (first decl)) (list decl) decl))))

(defn parse-clause [clause]
  (if (seq? clause) (set clause) #{clause}))

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
      (CljsVar. (no-warn #{:undeclared-ns} (cljs/resolve-var env sym (cljs/confirm-var-exists-throw)))))
    (catch Exception _e
      (when-some [v (cljs/resolve-macro-var env sym)]
        (CljsVar. v)))))

(defn resolve-var 
  "Resolve a clojure or clojurescript var. 
   If the resolved clojurescript var is a macro var, return the corresponding clojure var.
   Return an IVar or nil."
  [env sym]
  (if (:js-globals env)
    (let [var (resolve-cljs env sym)] ; resolve cljs var decription (a map)
      (if-let [expander (cljs/get-expander (if (some? var) (var-name var) sym) env)] ; find corresponding clojure var
        (CljVar. expander)
        var))
    (let [ns       (resolve-ns (:ns env)) ; current ns
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
          (CljClass. resolved))))))

(defn resolve-runtime
  "Returns the fully qualified symbol of the var resolved by given symbol at runtime, or nil if:
  - it cannot be resolved,
  - it doesn't exist at runtime (is a macro),
  - sym is a special form."
  [env sym]
  (letfn [(runtime-symbol [var] (when (some? var)
                                  (when-not (or (is-macro var) (is-node var))
                                    (var-name var))))]
    (if (:js-globals env)
      (if-let [v (resolve-var env sym)]
        (cond (instance? CljsVar v) (runtime-symbol v)
              ;; GG: if sym resolves to a clojure var, look up for the cljs-specific version.
              ;;     Why: some vars exist in two versions (e.g. cljs.core/inc)
              ;;          - the cljs version is a function (available at runtime),
              ;;          - the clj version is a macro expending to optimized code (compile-time only).
              (instance? CljVar v) (runtime-symbol (resolve-cljs env sym)))
        ;; GG: corner case: there is no var for cljs.core/unquote-splicing.
        (when (= 'cljs.core/unquote-splicing (:name (cljs/resolve-var env sym)))
          'cljs.core/unquote-splicing))
      (runtime-symbol (resolve-var env sym)))))

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

(defn analyze-global [sym]
  (case (namespace sym)
    "js"
    [:eval sym]
    [:global (global sym)]))

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
      (throw (ex-info (str "Not a reactive var - " (var-name var)) {})))
    (throw (ex-info (str "Unable to resolve symbol - " sym) {}))))

(defn analyze-symbol [env sym]
  (if (contains? (:locals env) sym)
    (if-some [[p i] (::pub (get (:locals env) sym))]
      (let [s [:sub (- (get (::index env) p) i)]]
        (if (= p (::local env))
          [s] [[:output s] [:input]]))
      [[:global (keyword sym)]])
    (if-some [sym (resolve-runtime env sym)]
      [(analyze-global sym)]
      (if-some [v (resolve-var env sym)]
        (if (is-node v)
          [[:node (visit-node env (var-name v) (::node (var-meta v)))]]
          (throw (ex-info "Can't take value of macro." {:symbol (var-name v)})))
        [(analyze-global (resolve-sym env sym))] ; pass through
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
           (conj-res (rec (update env ::index update l (fnil inc 0)) bs (conj ns node))))
         (reduce-kv
           (fn [res i n]
             (conj-res [[:bind (resolve-node env n) (- (count ns) i)]] res))
           (apply f env args) ns))) env (seq bs) [])))

(defn desugar-host [env form]
  (if (:js-globals env)
    form ;; Can't desugar in cljs, pass form through.
    (if (and (seq? form) (qualified-symbol? (first form)))
      (env/with-env {:namespaces {(:ns env) (resolve-ns (:ns env))}}
        (clj/desugar-host-expr form env))
      (clj/desugar-host-expr form env))))

(defn analyze-sexpr [env [op & args :as form]]
  (case op
    (fn* letfn* loop* recur set! ns ns* deftype* defrecord* var)
    (throw (ex-info "Unsupported operation." {:op op :args args}))

    (let*)
    ((fn rec [env bs]
       (if-some [[s i & bs] bs]
         (-> [[:pub]]
           (conj-res (analyze-form env i))
           (conj-res (rec (with-local env s) bs)))
         (analyze-sexpr env (cons `do (next args)))))
     env (seq (first args)))

    (do)
    (if-some [[x & xs] args]
      (case xs
        nil (analyze-form env x)
        (analyze-sexpr env (list {} x (cons `do xs))))
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
        (->> (when total? (list ::closure (peek clauses)))
          (list
            (reduce merge {}
              (map (fn [p s] (zipmap (parse-clause (first p)) (repeat s)))
                partition symbols))
            (first args))
          (list `let (vec (interleave symbols
                            (map (fn [p] (list ::closure (second p)))
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
        [[:eval `(js-call ~f ~(count args))]] args)
      (throw (ex-info "Wrong number of arguments - js*" {})))

    (new)                                                   ; argument binding + monadic join
    (if-some [[f & args] args]
      (if-some [ctor (when (symbol? f)                      ; detect clj/cljs class
                       (when-not (contains? (:locals env) f)
                         (resolve-runtime env f)))]

        ; clj/cljs class interop
        (transduce (map (partial analyze-form env)) conj-res
          [[:apply [:eval `(ctor-call ~ctor ~(count args))]]] args)

        ; boot signal (monadic join), with arguments passed as dynamic scope
        (analyze-binding env (interleave arg-sym (cons f args))
          (fn [_] [[:source] [:variable [:sub (inc (count args))]]])))
      (throw (ex-info "Wrong number of arguments - new" {})))

    (.)
    (let [dot (cljs/build-dot-form [(first args) (second args) (nnext args)])]
      (transduce (map (partial analyze-form env)) conj-res
        [[:eval (case (:dot-action dot)
                  ::cljs/call   `(method-call ~(:method dot) (count args))
                  ::cljs/access `(field-access ~(:field dot)))]]
        (cons (:target dot) (:args dot))))

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
            [[] () nil] args)]
      (analyze-form env
        `(new ~(reduce
                 (fn [r f] `(r/latest-first ~r (::closure ~f)))
                 (case catches
                   [] `(::closure (do ~@forms))
                   `(r/recover
                      (some-fn
                        ~@(map (fn [[c s & body]]
                                 (let [f `(partial (def exception) (::closure (let [~s exception] ~@body)))]
                                   (case c
                                     (:default Throwable)
                                     `(r/clause ~f)
                                     `(r/clause ~f ~c)))) catches))
                      (::closure (do ~@forms)))) finally))))

    (::closure)
    (let [res (analyze-form env (first args))]
      [[:target ((void [:nop]) (pop res))] [:constant (peek res)]])

    (if (symbol? op)
      (let [n (name op)
            e (dec (count n))]
        (case (nth n e)
          \. (analyze-sexpr env `(new ~(symbol (namespace op) (subs n 0 e)) ~@args))
          (if (contains? (:locals env) op)
            (analyze-apply env form)
            (if-some [sym (resolve-runtime env op)]
              (case sym
                (clojure.core/unquote-splicing cljs.core/unquote-splicing)
                (let [res (analyze-form (update env ::local not) (first args))]
                  [[:output (peek res)] ((void [:input]) (pop res))])
                ;; else
                (analyze-apply env form))
              (if-some [v (resolve-var env op)]
                (case (var-name v)
                  (clojure.core/binding cljs.core/binding)
                  (analyze-binding env (first args) analyze-sexpr (cons `do (next args)))

                  (if (is-node v)
                    (analyze-apply env form)
                    (cond (instance? CljVar v) ; "manual" macroexpansion: call the var as a function, passing it &form and the appropriate &env
                          (analyze-form env (apply (get-var v) form (if (:js-globals env) env (:locals env)) args))

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
           (throw (ex-info (ex-message ex) (update (ex-data ex) :in (fnil conj []) form) (ex-cause ex))))
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
                   (if p (peek inst) ((void [:literal nil]) (pop inst)))))
               nodes)) [true false]))))

(tests

  (analyze {} '5) :=
  [[:literal 5] [:literal nil]]

  (analyze {} '(+ 2 3)) :=
  [[:apply [:global :clojure.core/+] [:literal 2] [:literal 3]]
   [:literal nil]]

  (analyze {} '(let [a 1] (+ a 2))) :=
  [[:pub [:literal 1] [:apply [:global :clojure.core/+] [:sub 1] [:literal 2]]]
   [:literal nil]]

  (analyze '{a nil} 'a) :=
  [[:global :a]
   [:literal nil]]

  (doto (def Ctor) (alter-meta! assoc :macro true ::node ::unbound))
  (analyze {} '(Ctor.)) :=
  [[:pub [:node 0]
    [:bind 1 1
     [:variable [:sub 1]]]]
   [:source [:literal nil]]]

  (analyze {} '~@:foo) :=
  [[:input]
   [:output [:literal :foo] [:literal nil]]]

  (analyze {} '(::closure :foo)) :=
  [[:constant [:literal :foo]]
   [:target [:nop] [:literal nil]]]

  (analyze {} '(do :a :b)) :=
  [[:apply [:apply [:global :clojure.core/hash-map]]
    [:literal :a] [:literal :b]]
   [:literal nil]]

  (analyze {} '(case 1 2 3 (4 5) ~@6 7)) :=
  [[:pub
    [:pub
     [:constant [:literal 3]]
     [:pub
      [:constant [:input]]
      [:apply
       [:apply [:global :clojure.core/hash-map]
        [:literal 2] [:sub 2]
        [:literal 4] [:sub 1]
        [:literal 5] [:sub 1]]
       [:literal 1]
       [:constant [:literal 7]]]]]
    [:bind 0 1 [:variable [:sub 1]]]]
   [:target [:nop]
    [:target [:output [:literal 6] [:nop]]
     [:target [:nop]
      [:source [:literal nil]]]]]]

  (analyze {} '(case 1 2 3 (4 5) ~@6)) :=
  [[:pub
    [:pub
     [:constant [:literal 3]]
     [:pub
      [:constant [:input]]
      [:apply
       [:apply [:global :clojure.core/hash-map]
        [:literal 2] [:sub 2]
        [:literal 4] [:sub 1]
        [:literal 5] [:sub 1]]
       [:literal 1]
       [:literal nil]]]]
    [:bind 0 1 [:variable [:sub 1]]]]
   [:target [:nop]
    [:target [:output [:literal 6] [:nop]]
     [:source [:literal nil]]]]]

  (doto (def foo) (alter-meta! assoc :macro true ::node nil))
  (doto (def bar) (alter-meta! assoc :macro true ::node 'foo))
  (analyze {} 'bar) :=
  [[:pub [:literal nil] [:bind 0 1 [:pub [:node 0] [:bind 1 1 [:node 1]]]]]
   [:literal nil]]

  (analyze {} '(def foo)) :=
  [[:pub [:literal nil] [:bind 0 1 [:def 0]]]
   [:literal nil]]

  (analyze {} '(let [a 1] (new ((def foo) (::closure ~@(new (::closure ~@foo))) (::closure a))))) :=
  [[:pub [:literal nil]
    [:bind 0 1
     [:pub [:literal 1]
      [:pub
       [:apply [:def 0]
        [:constant [:target [:output [:node 0] [:nop]] [:source [:input]]]]
        [:constant [:sub 1]]]
       [:bind 1 1 [:variable [:sub 1]]]]]]]
   [:target
    [:output [:pub [:constant [:input]] [:bind 0 1 [:variable [:sub 1]]]] [:nop]]
    [:target [:nop] [:source [:literal nil]]]]]

  (doto (def baz) (alter-meta! assoc :macro true ::node '(::closure ~@foo)))
  (analyze {} '(let [a 1] (new ((def foo) (::closure ~@(new baz)) (::closure a))))) :=
  [[:pub [:literal nil]
    [:bind 0 1
     [:target [:output [:node 0] [:nop]]
      [:pub [:literal 1]
       [:pub [:apply [:def 0] [:constant [:source [:input]]] [:constant [:sub 1]]]
        [:bind 1 1 [:variable [:sub 1]]]]]]]]
   [:pub [:constant [:input]]
    [:bind 0 1
     [:target [:output [:pub [:node 0] [:bind 1 1 [:variable [:sub 1]]]] [:nop]]
      [:target [:nop] [:source [:literal nil]]]]]]]

  (analyze {} '(try 1 (finally 2 3 4))) :=
  [[:pub [:apply
          [:global :hyperfiddle.photon-impl.runtime/latest-first]
          [:apply [:global :hyperfiddle.photon-impl.runtime/latest-first]
           [:apply [:global :hyperfiddle.photon-impl.runtime/latest-first]
            [:constant [:literal 1]] [:constant [:literal 2]]]
           [:constant [:literal 3]]]
          [:constant [:literal 4]]]
    [:bind 0 1 [:variable [:sub 1]]]]
   [:target [:nop]
    [:target [:nop]
     [:target [:nop]
      [:target [:nop]
       [:source [:literal nil]]]]]]]

  (analyze {} '(try 1 (catch Exception e 2) (finally 3))) :=
  [[:pub [:apply [:global :hyperfiddle.photon-impl.runtime/latest-first]
          [:apply [:global :hyperfiddle.photon-impl.runtime/recover]
           [:apply [:global :clojure.core/some-fn]
            [:apply [:global :hyperfiddle.photon-impl.runtime/clause]
             [:apply [:global :clojure.core/partial]
              [:def 0] [:constant [:pub [:node 0] [:literal 2]]]]
             [:global :java.lang.Exception]]]
           [:constant [:literal 1]]]
          [:constant [:literal 3]]]
    [:bind 1 1 [:variable [:sub 1]]]]
   [:target [:nop]
    [:target [:nop]
     [:target [:nop]
      [:source [:literal nil]]]]]]
  )


(tests "literals"
  (analyze {} {:a 1}) := [[:apply [:global :clojure.core/hash-map] [:literal :a] [:literal 1]] [:literal nil]]
  (analyze {} ^{:b 2} {:a 1}) := [[:apply
                                   [:global :clojure.core/with-meta]
                                   [:apply [:global :clojure.core/hash-map] [:literal :a] [:literal 1]]
                                   [:apply [:global :clojure.core/hash-map] [:literal :b] [:literal 2]]]
                                  [:literal nil]]
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
