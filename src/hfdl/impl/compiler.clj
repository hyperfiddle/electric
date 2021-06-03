(ns hfdl.impl.compiler
  (:require [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.utils :as utils]
            [missionary.core :as m]
            [hfdl.impl.util :as u]
            [hfdl.impl.switch :as s]
            [cljs.analyzer :as cljs]
            [hfdl.impl.runtime :as r]
            [hyperfiddle.rcf :refer [tests]])
  (:import clojure.lang.Compiler$LocalBinding
           cljs.tagged_literals.JSValue
           (clojure.lang Box Var)))

(def ^ThreadLocal context (ThreadLocal.))

(defn out [& args]
  (doseq [arg args]
    (.print System/out (pr-str arg))
    (.print System/out " "))
  (.println System/out))

;; GG: This is a copy/paste from `clojure.tools.analyser.jvm/macroexpand-1`, without inlining.
(defn macroexpand-1'
  "If form represents a macro form,returns its expansion, else returns form."
  ([form] (macroexpand-1' form (clj/empty-env)))
  ([form env]
   (env/ensure (clj/global-env)
     (cond
       (seq? form) (let [[op & _args] form]
                     (if (clj/specials op)
                       form
                       (let [v (utils/resolve-sym op env)
                             m (meta v)
                             local? (-> env :locals (get op))
                             macro? (and (not local?) (:macro m)) ;; locals shadow macros
                             ]
                         (if macro?
                           (let [res (apply v form (:locals env) (rest form))] ; (m &form &env & args)
                             (when-not (clj/ns-safe-macro v)
                               (clj/update-ns-map!))
                             (if (utils/obj? res)
                               (vary-meta res merge (meta form))
                               res))
                           (clj/desugar-host-expr form env)))))
       (symbol? form) (clj/desugar-symbol form env)
       :else form))))

(defn normalize-binding [binding]
  (if (instance? Compiler$LocalBinding binding)
    (let [binding ^Compiler$LocalBinding binding]
      {:op   :local
       :tag  (when (.hasJavaClass binding)
               (some-> binding (.getJavaClass)))
       :form (.-sym binding)
       :name (.-sym binding)})
    binding))

(defn normalize-env [env]
  (if (:js-globals env)
    env {:ns (ns-name *ns*) :locals env}))

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

(defn emit-fn [sym body arity]
  (let [arg-syms (into [] (map sym) (range arity))]
    (list `fn arg-syms (apply body arg-syms))))

(defn resolve-var [env sym]
  (let [ns-map (clj/build-ns-map)
        sym-ns (when-let [ns (namespace sym)]
                 (symbol ns))
        full-ns (when sym-ns
                  (or (get-in ns-map [(:ns env) :aliases sym-ns])
                    (:ns (ns-map sym-ns))))]
    (when (or (not sym-ns) full-ns)
      (let [name (if sym-ns (-> sym name symbol) sym)]
        (-> ns-map
          (get (or full-ns (:ns env)))
          :mappings (get name))))))

(defn runtime-resolve "
Returns the fully qualified symbol of the var resolved by given symbol at runtime, or nil if the var doesn't exist or
is a macro or special form."
  [env sym]
  (if (:js-globals env)
    (let [b (Box. true)
          v (cljs/resolve-var env sym
              (fn [env prefix suffix]
                (cljs/confirm-var-exists env prefix suffix
                  (fn [_ _ _] (set! (.-val b) false)))))]
      (when (.-val b) (:name v)))
    (when-some [^Var v (resolve-var env sym)]
      (when-not (.isMacro v) (.toSymbol v)))))

(defn with-local [env sym]
  (let [peer (:peer (.get context))
        tier (peer (::tier env))]
    (-> env
      (update ::tier assoc peer (inc tier))
      (update :locals update sym assoc
        :name sym ::peer peer ::tier tier))))

(defn analyze-local [sym loc]
  (if-some [p (::peer loc)]
    (let [node [:sub (::tier loc)]]
      (if (= p (:peer (.get context)))
        [node [:void]] [[:input [:void]] [:output node]]))
    [[:literal sym] [:void]]))

(defn analyze-global [sym]
  (case (namespace sym)
    "js"
    [:interop (constantly sym)]
    [:global (global sym)]))

(defn analyze-symbol [env sym]
  (if-some [loc (get (:locals env) sym)]
    (analyze-local sym loc)
    (if-some [sym (runtime-resolve env sym)]
      [(analyze-global sym) [:void]]
      ;; TODO (clj/desugar-symbol form env)
      (throw (ex-info "Unable to resolve symbol." {:symbol sym})))))

(defn toggle! []
  (.set context (update (.get context) :peer {:client :server, :server :client})))

(def analyze
  (letfn [(map-forms [env f & forms]
            (let [x (map (partial analyze-form env) forms)]
              [(apply f (map first x)) (apply vector :void (map second x))]))
          (analyze-binding [[env & res] [name init]]
            (cons (with-local env name) (cons (analyze-form env init) res)))
          (analyze-sexpr [env [op & args :as form]]
            (if (symbol? op)
              (if-some [loc (get (:locals env) op)]
                (let [[l r] (analyze-local op loc)
                      x (map (partial analyze-form env) args)]
                  [(apply vector :invoke l (map first x))
                   (apply vector :void r (map second x))])
                (case op
                  (var throw try def fn* letfn* loop* recur set! ns ns* deftype* defrecord*)
                  (throw (ex-info "Unsupported operation." {:op op :args args}))

                  (let*)
                  (let [[env & res] (reduce analyze-binding (list env) (partition 2 (first args)))]
                    (reduce (fn [[l r] [sl sr]] [[:pub sl l] [:void sr r]])
                      (analyze-sexpr env (cons `do (next args))) res))

                  (do)
                  (if-some [[x & xs] args]
                    (case xs
                      nil (analyze-form env x)
                      (analyze-sexpr env (list `case x (cons `do xs))))
                    [[:literal nil] [:void]])

                  (if)
                  (let [[test then else] args]
                    (analyze-form env `(case ~test (nil false) ~else ~then)))

                  (case clojure.core/case cljs.core/case)
                  (let [[tl tr] (analyze-form env (first args))
                        clauses (vec (next args))]
                    (if (odd? (count clauses))
                      (let [[dl dr] (analyze-form env (peek clauses))]
                        (transduce
                          (partition-all 2)
                          (completing
                            (fn [[cl cr] [t x]]
                              (let [[bl br] (analyze-form env x)]
                                [(conj cl (conj (if (seq? t) (vec t) [t]) bl))
                                 (conj cr [:frame br])])))
                          [[:case tl dl] [:void tr [:frame dr]]] (pop clauses)))
                      (throw (ex-info "Unsupported operation : case without default."
                               {:op op :args args}))))

                  (quote)
                  [[:literal form] [:void]]

                  (js* new)
                  (apply map-forms env (partial vector :interop (partial list op (first args))) (next args))

                  (.)
                  (let [dot (cljs/build-dot-form [(first args) (second args) (nnext args)])]
                    (apply map-forms env
                      (partial vector :interop
                        (case (:dot-action dot)
                          ::cljs/call (fn [target & args] `(. ~target ~(:method dot) ~@args))
                          ::cljs/access (fn [target] `(. ~target ~(symbol (str "-" (name (:field dot))))))))
                      (cons (:target dot) (:args dot))))

                  (if-some [sym (runtime-resolve env op)]
                    (case sym
                      (clojure.core/unquote cljs.core/unquote)
                      (map-forms env (partial vector :variable) (first args))

                      (clojure.core/unquote-splicing cljs.core/unquote-splicing)
                      (try (toggle!)
                           (let [[l r] (analyze-form env (first args))]
                             [[:input r] [:output l]])
                           (finally (toggle!)))

                      (hfdl.impl.runtime/prod)
                      (let [[pl pr] (analyze-form (reduce with-local env (keys (first args))) (second args))
                            [il ir] (apply map-forms env (partial vector :product pl) (vals (first args)))]
                        [il [:void ir [:frame pr]]])

                      (hfdl.impl.runtime/node)
                      (let [[l r] (apply map-forms env (partial vector :node (first args)) (next args))]
                        [l [:void r [:node (first args)]]])

                      (apply map-forms env (partial vector :invoke (analyze-global sym)) args))

                    (analyze-form env
                      (if (:js-globals env)
                        (cljs/macroexpand-1 env form)
                        (if-some [v (resolve-var env op)]
                          (apply v form (:locals env) args)
                          (clj/desugar-host-expr form env)))))))

              (apply map-forms env (partial vector :invoke) form)))
          (analyze-map [env form]
            (analyze-form env (cons `hash-map (sequence cat form))))
          (analyze-set [env form]
            (analyze-form env (cons `hash-set form)))
          (analyze-vector [env form]
            (analyze-form env (cons `vector form)))
          (analyze-js [env form]
            (assert nil "Not implemented : #js"))
          (analyze-form [env form]
            (if-let [analyze
                      (or
                        (and (symbol? form) analyze-symbol)
                        (and (seq? form) (seq form) analyze-sexpr)
                        (and (map? form) analyze-map)
                        (and (vector? form) analyze-vector)
                        (and (set? form) analyze-set)
                        (and (instance? JSValue form) analyze-js))]
              (analyze env form) [[:literal form] [:void]]))] analyze-form))

(defn reset-tiers [env]
  (assoc env ::tier {:client 0 :server 0}))

(tests

  (defn ir [form]
    (let [prv (.get context)]
      (.set context {:peer :client})
      (try (analyze (reset-tiers (normalize-env nil)) form)
           (finally (.set context prv)))))

  (ir 5) :=
  [[:literal 5] [:void]]

  (ir '(+ 2 3)) :=
  [[:invoke [:global :clojure.core/+] [:literal 2] [:literal 3]]
   [:void [:void] [:void]]]

  (ir '(let [a 1] (+ a 2))) :=
  [[:pub [:literal 1] [:invoke [:global :clojure.core/+] [:sub 0] [:literal 2]]]
   [:void [:void] [:void [:void] [:void]]]]

  (ir '~m/none) :=
  [[:variable [:global :missionary.core/none]] [:void [:void]]]

  (ir '~@:foo) :=
  [[:input [:void]] [:output [:literal :foo]]]

  (ir '(case 1 2 3 (4 5) ~@6 7)) :=
  [[:case [:literal 1] [:literal 7] [2 [:literal 3]] [4 5 [:input [:void]]]]
   [:void [:void] [:frame [:void]] [:frame [:void]] [:frame [:output [:literal 6]]]]]

  (ir '(r/prod {a [:foo]} [a ~@:bar])) :=
  [[:product
    [:invoke [:global :clojure.core/vector] [:sub 0] [:input [:void]]]
    [:invoke [:global :clojure.core/vector] [:literal :foo]]]
   [:void
    [:void [:void [:void]]]
    [:frame [:void [:void] [:output [:literal :bar]]]]]]


  )

(defn emit-log-failure [form]
  `(u/log-failure ~form))

(defn emit-node [sym {:keys [peer arity id local remote]}]
  (list
    (sym 'node (or id ""))
    (into [(sym 'path)] (map sym) (case peer :client (range arity) :server []))
    ((fn walk [tier [op & args]]
       (case op
         :sub (sym (first args))
         :pub `(let [~(sym tier) (r/share ~(sym 'ctx) ~(sym 'path) ~(walk tier (first args)))]
                 ~(walk (inc tier) (second args)))
         :node `(~(sym 'node (first args)) ~(sym 'path)
                  ~@(map (fn [inst] `(r/share ~(sym 'ctx) ~(sym 'path) ~(walk tier inst))) (next args)))
         :void `(do ~@(map (partial walk tier) args) nil)
         :case (let [branches (map (partial sym 'branch) (range))]
                 `(r/branch ~(sym 'ctx) ~(walk tier (first args))
                    (let [~@(interleave
                              (cons (sym 'branch) branches)
                              (map (fn [x]
                                     `(r/inner ~(sym 'ctx) ~(sym 'path)
                                        (fn [~(sym 'path)] ~(walk tier x))))
                                (cons (second args) (map peek (nnext args)))))]
                      (fn [~(sym 'test)]
                        (case ~(sym 'test)
                          ~@(mapcat
                              (partial mapcat vector)
                              (map pop (nnext args))
                              (map repeat branches))
                          ~(sym 'branch))))))
         :frame `(r/inner ~(sym 'ctx) ~(sym 'path)
                   (fn [~(sym 'path)] ~(walk tier (first args))))
         :input `(do ~(walk tier (first args)) (r/input ~(sym 'ctx) ~(sym 'path)))
         :output `(r/output ~(sym 'ctx) ~(sym 'path) ~(walk tier (first args)))
         :invoke `(r/invoke ~@(map (partial walk tier) args))
         :global `(r/steady ~(symbol (first args)))
         :product `(r/product ~(sym 'ctx) ~(sym 'path)
                     ~(let [after (+ tier (dec (count args)))]
                        `(fn [~(sym 'path) ~@(map sym (range tier (+ tier after)))]
                           ~(walk after (first args))))
                     ~@(map (partial walk tier) (next args)))
         :literal `(r/steady ~(first args))
         :interop `(m/latest
                     ~(emit-fn sym (first args) (count (next args)))
                     ~@(map (partial walk tier) (next args)))
         :constant `(r/steady ~(walk tier (first args)))
         :variable `(s/switch ~(walk tier (first args)))))
     arity (case peer :client local :server remote))))

(defn node [ns name decl args]
  (if-some [{:keys [peer nodes] :as ctx} (.get context)]
    (let [node-key {:ns ns :name name :peer peer :arity (count args)}]
      (when-not (contains? nodes node-key)
        (.set context (assoc ctx :nodes (assoc nodes node-key {:id (count nodes)})))
        (let [[syms expr] (if (vector? (first decl))
                            [(first decl) (cons `do (next decl))]
                            (throw (ex-info "Can't build node : unsupported declaration."
                                     {:ns ns :name name :decl decl :args args})))
              [l r] (analyze
                      (reduce with-local
                        (reset-tiers
                          {:ns     (symbol ns)
                           :locals {}}) syms) expr)]
          (.set context (update-in (.get context) [:nodes node-key] assoc :local l :remote r))))
      `(r/node ~(get-in (.get context) [:nodes node-key :id]) ~@args))
    (throw (ex-info "Can't build node : not in dataflow context."
             {:ns ns :name name :decl decl :args args}))))

(defn main [prefix env form]
  (let [sym (comp symbol (partial str prefix '-))
        prv (.get context)]
    (.set context {:peer :client})
    (try
      (let [[l r] (analyze (reset-tiers (normalize-env env)) form)
            nodes (-> []
                    (into (->> (:nodes (.get context))
                            (map (partial apply merge))
                            (sort-by :id)))
                    (conj {:peer :client :arity 0 :local l :remote r}))]
        `[(fn [~(sym 'ctx)]
            (letfn [~@(map (partial emit-node sym) nodes)]
              (~(sym 'node) [])))
          ~(mapv (fn [{:keys [peer local remote]}]
                   (case peer :client remote :server local)) nodes)])
      (finally (.set context prv)))))

(tests
  (main (symbol "") nil '4) :=
  `[(fn [~'-ctx] (letfn [(~'-node [~'-path] (r/steady 4))] (~'-node []))) [[:void]]]

  )


(defn runtime-resolve-throwing [env form]
  (when-not (symbol? form) (throw (ex-info "Symbol expected." {:form form})))
  (doto (runtime-resolve env form)
    (-> (when-not (throw (ex-info "Unable to resolve symbol." {:form form}))))))

(defn vars [env forms]
  (into {} (map (comp (juxt global identity)
                  (partial runtime-resolve-throwing
                    (normalize-env env)))) forms))