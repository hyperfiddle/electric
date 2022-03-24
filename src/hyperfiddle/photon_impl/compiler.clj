(ns hyperfiddle.photon-impl.compiler
  (:require [clojure.tools.analyzer.jvm :as clj]
            [missionary.core :as m]
            [cljs.analyzer :as cljs]
            [hyperfiddle.photon-impl.local :as l]
            [hyperfiddle.rcf :refer [tests]]
            [cljs.analyzer.api :as api])
  (:import cljs.tagged_literals.JSValue
           (clojure.lang Box Var)))

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

(defn desugar-host [env form]
  (if (:js-globals env)
    (throw (ex-info "Unsupported form." {:form form}))
    (clj/desugar-host-expr form env)))

(defn resolve-var [env sym]
  (if (:js-globals env)
    (cljs/get-expander sym env)
    (let [ns-map (clj/build-ns-map)
          sym-ns (when-let [ns (namespace sym)]
                   (symbol ns))
          full-ns (when sym-ns
                    (or (-> ns-map (get (:ns env)) (get :aliases) (get sym-ns))
                        (:ns (ns-map sym-ns))))]
      (when (or (not sym-ns) full-ns)
        (let [name        (if sym-ns (-> sym name symbol) sym)
              mapped-name (-> ns-map
                              (get (or full-ns (:ns env)))
                              :mappings (get name))]
          (if (some? mapped-name)
            mapped-name
            ;; java.lang is implicit so not listed in ns form or env
            (clojure.lang.Compiler/maybeResolveIn (the-ns (:ns env)) sym)))))))

(defn resolve-runtime "
Returns the fully qualified symbol of the var resolved by given symbol at runtime, or nil if the var doesn't exist or
is a macro or special form."
  [env sym]
  (if (:js-globals env)
    (let [b (Box. true)
          v (cljs/resolve-var env sym
              (fn [env prefix suffix]
                (cljs/confirm-var-exists env prefix suffix
                  (fn [_ _ _] (when-not ((symbol (str prefix) (str suffix)) '#{cljs.core/unquote-splicing})
                                (set! (.-val b) false))))))]
      (when (.-val b) (when-not (:macro v) (:name v))))
    (let [v (resolve-var env sym)]
      (or (when (instance? Var v) (when-not (or (.isMacro ^Var v)
                                                (::node (meta v)))
                                    (.toSymbol ^Var v)))
        (when (instance? Class v) (symbol (.getName ^Class v)))))))

(defn with-local [env sym]
  (let [l (::local env)
        i (get (::index env) l 0)]
    (-> env
      (update ::index assoc l (inc i))
      (update :locals assoc sym {::pub [l i]}))))

(defn analyze-global [sym]
  (case (namespace sym)
    "js"
    [:interop (constantly sym)]
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
  (let [clj-var  (resolve-var env sym)
        cljs-var (when-not (some? clj-var) (api/resolve env sym))]
    (if (or (some? clj-var) (some? cljs-var))
      (let [m        (if (some? clj-var)
                       (meta clj-var)
                       (:meta cljs-var))
            s        (if (some? cljs-var)
                       (:name cljs-var) ;; cljs analyzer ast
                       (symbol (name (ns-name (:ns m))) (name (:name m))))]
        (if-some [n (::node m)]
          (visit-node env s n)
          (throw (ex-info (str "Not a reactive var - " s) {}))))
      (throw (ex-info (str "Unable to resolve symbol - " sym) {})))))

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
        (let [m (meta v)
              s (symbol (name (ns-name (:ns m))) (name (:name m)))]
          (if-some [n (::node m)]
            [[:node (visit-node env s n)]]
            (throw (ex-info "Can't take value of macro." {:symbol s}))))
        (throw (ex-info "Unable to resolve symbol." {:symbol sym}))))))

(defn analyze-apply [env form]
  (transduce
   (map (partial analyze-form env))
   conj-res [[:apply]] form))

(defn analyze-sexpr [env [op & args :as form]]
  (if (and (symbol? op) (not (contains? (:locals env) op)))
    (case op
      (fn* letfn* loop* recur set! ns ns* deftype* defrecord*)
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
                      (->> (when total? (list `var (peek clauses)))
                           (list
                            (reduce merge {}
                                    (map (fn [p s] (zipmap (parse-clause (first p)) (repeat s)))
                                         partition symbols))
                            (first args))
                           (list `let (vec (interleave symbols
                                                       (map (fn [p] (list `var (second p)))
                                                            partition))))
                           (list `unquote))))

      (quote)
      [[:literal (first args)]]

      (def)
      (transduce (map (comp vector (partial resolve-node env)))
                 conj-res [[:def]] args)

      (var)
      (let [res (analyze-form env (first args))]
        [[:target ((void [:nop]) (pop res))] [:constant (peek res)]])

      (js* new)
      (transduce (map (partial analyze-form env)) conj-res
                 [[:interop (partial list op (first args))]] (next args))

      (.)
      (let [dot (cljs/build-dot-form [(first args) (second args) (nnext args)])]
        (transduce (map (partial analyze-form env)) conj-res
                   [[:interop (case (:dot-action dot)
                                ::cljs/call   (fn [target & args] `(. ~target ~(:method dot) ~@args))
                                ::cljs/access (fn [target] `(. ~target ~(symbol (str "-" (name (:field dot)))))))]]
                   (cons (:target dot) (:args dot))))

      (throw)
      (conj-res [[:error]] (analyze-form env (first args)))

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
             [[] () nil] args)
            body (analyze-form env `(do ~@forms))]
        (reduce
         (fn [r stmt]
           (conj-res [[:first]] r (analyze-form env stmt)))
         (case catches
           [] body
           (let [e (gensym)
                 r (analyze-form (with-local env e)
                                 (reduce
                                  (fn [cont [c s & body]]
                                    (let [form `(let [~s ~e] ~@body)]
                                      (case c :default form `(if (instance? ~c ~e) ~form ~cont))))
                                  `(throw ~e) catches))]
             (conj (pop body)
                   [:target ((void [:nop]) (pop r))] [:source]
                   [:recover (peek body) (peek r)]))) finally))

      (if-some [sym (resolve-runtime env op)]
        (case sym
          (clojure.core/unquote cljs.core/unquote)
          (let [res (analyze-form env (first args))]
            (conj (conj (pop res) [:source]) [:variable (peek res)]))

          (clojure.core/unquote-splicing cljs.core/unquote-splicing)
          (let [res (analyze-form (update env ::local not) (first args))]
            [[:output (peek res)] ((void [:input]) (pop res))])

          (analyze-apply env form))
        (if-some [v (resolve-var env op)]
          (let [m (meta v)
                s (symbol (name (ns-name (:ns m))) (name (:name m)))]
            (case s
              (clojure.core/binding cljs.core/binding)
              ((fn rec [env ns bs]
                 (if-some [[n i & bs] bs]
                   (let [s (gensym)]
                     (-> [[:pub]]
                         (conj-res (analyze-form env i))
                         (conj-res (rec (with-local env s) (cons [n s] ns) bs))))
                   (reduce
                    (fn [res [n s]]
                      (-> [[:bind (resolve-node env n)]]
                          (conj-res (analyze-symbol env s))
                          (conj-res res)))
                    (analyze-sexpr env (cons `do (next args))) ns)))
               env nil (seq (first args)))

              (if (::node m)
                (analyze-apply env form)
                (analyze-form env (apply v form (if (:js-globals env) env (:locals env)) args)))))
          (let [desugared-form (desugar-host env form)]
            (if (= form desugared-form)
              ;; Nothing got desugared, this is not host interop, meaning `op` var wasn't found.
              (throw (ex-info "Unable to resolve symbol" {:symbol op}))
              (analyze-form env desugared-form))))))
    (analyze-apply env form)))

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
    (analyze env form) [[:literal form]]))

(let [nodes (l/local)]
  (defn visit-node [env sym form]                      ;; TODO detect cycles
    (let [l (::local env)]
      (if-some [node (-> (.get nodes) (get l) (get sym))]
        (:slot node)
        (let [sym-ns (symbol (namespace sym))
              inst (-> env
                       (assoc :locals {} :ns (if (:js-globals env) (cljs/get-namespace sym-ns) sym-ns))
                       (dissoc ::index)
                       (analyze-form (if (and (seq? form) (= 'quote (first form)))
                                       (second form)
                                       form)))
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
                     [:bind slot (peek inst) (rec nodes)]
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

  (analyze {} '~m/none) :=
  [[:variable [:global :missionary.core/none]]
   [:source [:literal nil]]]

  (analyze {} '~@:foo) :=
  [[:input]
   [:output [:literal :foo] [:literal nil]]]

  (analyze {} '#':foo) :=
  [[:constant [:literal :foo]]
   [:target [:nop] [:literal nil]]]

  (analyze {} '(do :a :b)) :=
  [[:apply [:apply [:global :clojure.core/hash-map]]
    [:literal :a] [:literal :b]]
   [:literal nil]]

  (analyze {} '(case 1 2 3 (4 5) ~@6 7)) :=
  [[:variable
    [:pub [:constant [:literal 3]]
     [:pub [:constant [:input]]
      [:apply [:apply [:global :clojure.core/hash-map]
               [:literal 2] [:sub 2]
               [:literal 4] [:sub 1]
               [:literal 5] [:sub 1]]
       [:literal 1]
       [:constant [:literal 7]]]]]]
   [:target [:nop]
    [:target [:output [:literal 6] [:nop]]
     [:target [:nop] [:source [:literal nil]]]]]]

  (analyze {} '(case 1 2 3 (4 5) ~@6)) :=
  [[:variable
    [:pub
     [:constant [:literal 3]]
     [:pub
      [:constant [:input]]
      [:apply [:apply [:global :clojure.core/hash-map]
               [:literal 2] [:sub 2]
               [:literal 4] [:sub 1]
               [:literal 5] [:sub 1]]
       [:literal 1]
       [:literal nil]]]]]
   [:target [:nop]
    [:target [:output [:literal 6] [:nop]]
     [:source [:literal nil]]]]]

  (doto (def foo) (alter-meta! assoc :macro true ::node '(do)))
  (doto (def bar) (alter-meta! assoc :macro true ::node '(do foo)))
  (analyze {} 'bar) :=
  [[:bind 0 [:literal nil] [:bind 1 [:node 0] [:node 1]]]
   [:literal nil]]

  (analyze {} '(def foo)) :=
  [[:bind 0 [:literal nil] [:def 0]]
   [:literal nil]]

  (analyze {} '(let [a 1] ~((def foo) #'~@~#'~@foo #'a))) :=
  [[:bind 0 [:literal nil]
    [:pub [:literal 1]
     [:variable
      [:apply [:def 0]
       [:constant [:target [:output [:node 0] [:nop]] [:source [:input]]]]
       [:constant [:sub 1]]]]]]
   [:target [:output [:variable [:constant [:input]]] [:nop]]
    [:target [:nop]
     [:source [:literal nil]]]]]

  (doto (def baz) (alter-meta! assoc :macro true ::node '(do #'~@foo)))
  (analyze {} '(let [a 1] ~((def foo) #'~@~baz #'a))) :=
  [[:bind 0 [:literal nil]
    [:target [:output [:node 0] [:nop]]
     [:pub [:literal 1]
      [:variable
       [:apply [:def 0]
        [:constant [:source [:input]]]
        [:constant [:sub 1]]]]]]]
   [:bind 0 [:constant [:input]]
    [:target [:output [:variable [:node 0]] [:nop]]
     [:target [:nop] [:source [:literal nil]]]]]]

  (analyze {} '(try 1 (finally 2 3 4))) :=
  [[:first [:first [:first [:literal 1] [:literal 2]] [:literal 3]] [:literal 4]] [:literal nil]]

  (analyze {} '(try 1 (catch Exception e 2) (finally 3))) :=
  [[:first
    [:recover
     [:literal 1]
     [:variable
      [:pub
       [:constant [:error [:sub 1]]]
       [:apply
        [:apply [:global :clojure.core/hash-map] [:literal nil] [:sub 1] [:literal false] [:sub 1]]
        [:apply [:global :clojure.core/instance?] [:global :java.lang.Exception] [:sub 2]]
        [:constant [:pub [:sub 2] [:literal 2]]]]]]]
    [:literal 3]]
   [:target [:target [:nop] [:target [:nop] [:source [:nop]]]] [:source [:literal nil]]]]

  )


(tests "literals"
  (analyze {} {:a 1}) := [[:apply [:global :clojure.core/hash-map] [:literal :a] [:literal 1]] [:literal nil]]
  (analyze {} ^{:b 2} {:a 1}) := [[:apply
                                   [:global :clojure.core/with-meta]
                                   [:apply [:global :clojure.core/hash-map] [:literal :a] [:literal 1]]
                                   [:apply [:global :clojure.core/hash-map] [:literal :b] [:literal 2]]]
                                  [:literal nil]]
  )

(def args
  (map (comp symbol
         (partial intern *ns*)
         (fn [i]
           (with-meta (symbol (str "%" i))
             {:macro true ::node ()})))
    (range)))

(defn resolve-runtime-throwing [env form]
  (when-not (symbol? form) (throw (ex-info "Symbol expected." {:form form})))
  (doto (resolve-runtime env form)
    (-> (when-not (throw (ex-info "Unable to resolve symbol." {:form form}))))))

(defn vars [env forms]
  (let [resolved (into {} (map (comp (juxt global identity)
                                     (partial resolve-runtime-throwing (normalize-env env))))
                       forms)]
    `(fn ~'global-var-resolver
       ([ident#]
        (contains? ~(set (keys resolved)) ident#))
       ([not-found# ident#]
        (case ident#
          ~@(mapcat identity resolved)
          not-found#)))))

