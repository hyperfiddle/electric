(ns hfdl.impl.compiler
  (:require [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.utils :as utils]
            [missionary.core :as m]
            [hfdl.impl.util :as u]
            [cljs.analyzer :as cljs]
            [hfdl.impl.runtime :as r]
            [hyperfiddle.rcf :refer [tests]])
  (:import clojure.lang.Compiler$LocalBinding
           cljs.tagged_literals.JSValue
           (clojure.lang Box Var)))

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
                    (or (get-in ns-map [(:ns env) :aliases sym-ns])
                      (:ns (ns-map sym-ns))))]
      (when (or (not sym-ns) full-ns)
        (let [name (if sym-ns (-> sym name symbol) sym)]
          (-> ns-map
            (get (or full-ns (:ns env)))
            :mappings (get name)))))))

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
    (when-some [^Var v (resolve-var env sym)]
      (when-not (.isMacro v) (.toSymbol v)))))

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

(def analyze
  (let [nodes (ThreadLocal.)]
    (letfn [(conj-res
              ([x] x)
              ([x y]
               (conj (into (pop x) (pop y))
                 (conj (peek x) (peek y)))))
            (visit-node [env sym body]
              (let [l (::local env)]
                (if-some [node (-> (.get nodes) (get l) (get sym))]
                  (doto (:slot node) (when-not (throw (ex-info (str "Circular dependency - " sym) {}))))
                  (do
                    (.set nodes (update (.get nodes) l assoc sym {}))
                    (-> env
                      (assoc :locals {} :ns (symbol (namespace sym)))
                      (dissoc ::index)
                      (analyze-form (cons `do body))
                      (->> (update (.get nodes) l update sym assoc :inst) (.set nodes)))
                    (doto (-> (.get nodes) (get l) count dec)
                      (->> (update (.get nodes) l update sym assoc :slot) (.set nodes)))))))
            (resolve-node [env sym]
              (doto (when-some [v (resolve-var env sym)]
                      (let [m (meta v)]
                        (when (contains? m :node)
                          (visit-node env
                            (symbol (name (ns-name (:ns m)))
                              (name (:name m))) (:node m)))))
                (when-not (throw (ex-info (str "Unable to resolve node - " sym) {})))))
            (analyze-symbol [env sym]
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
                      (if-some [n (:node m)]
                        [[:node (visit-node env s n)]]
                        (throw (ex-info "Can't take value of macro." {:symbol sym}))))
                    (throw (ex-info "Unable to resolve symbol." {:symbol sym}))))))
            (analyze-apply [env form]
              (transduce
                (map (partial analyze-form env))
                conj-res [[:apply]] form))
            (analyze-sexpr [env [op & args :as form]]
              (if (and (symbol? op) (not (contains? (:locals env) op)))
                (case op
                  (throw try fn* letfn* loop* recur set! ns ns* deftype* defrecord*)
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
                      (analyze-sexpr env (list `case x (cons `do xs))))
                    [[:literal nil]])

                  (if)
                  (let [[test then else] args]
                    (analyze-form env `(case ~test (nil false) ~else ~then)))

                  (case clojure.core/case cljs.core/case)
                  (analyze-form env
                    (let [clauses (vec (next args))]
                      (if (odd? (count clauses))
                        (let [partition (partition-all 2 (pop clauses))
                              symbols (repeatedly gensym)]
                          (->> (list `var (peek clauses))
                            (list
                              (reduce merge {}
                                (map (fn [p s] (zipmap (parse-clause (first p)) (repeat s)))
                                  partition symbols))
                              (first args))
                            (list `let (vec (interleave symbols
                                              (map (fn [p] (list `var (second p)))
                                                partition))))
                            (list `unquote)))
                        (throw (ex-info "TODO partial case" {})))))

                  (quote)
                  [[:literal form]]

                  (def)
                  (transduce (map (comp vector (partial resolve-node env)))
                    conj-res [[:def]] args)

                  (var)
                  (let [res (analyze-form env (first args))]
                    [(into [:target] (pop res)) [:constant (peek res)]])

                  (js* new)
                  (transduce (map (partial analyze-form env)) conj-res
                    [[:interop (partial list op (first args))]] (next args))

                  (.)
                  (let [dot (cljs/build-dot-form [(first args) (second args) (nnext args)])]
                    (transduce (map (partial analyze-form env)) conj-res
                      [[:interop (case (:dot-action dot)
                                   ::cljs/call (fn [target & args] `(. ~target ~(:method dot) ~@args))
                                   ::cljs/access (fn [target] `(. ~target ~(symbol (str "-" (name (:field dot)))))))]]
                      (cons (:target dot) (:args dot))))

                  (if-some [sym (resolve-runtime env op)]
                    (case sym
                      (clojure.core/unquote cljs.core/unquote)
                      (conj-res [[:variable]] (analyze-form env (first args)))
                      (clojure.core/unquote-splicing cljs.core/unquote-splicing)
                      (let [res (analyze-form (update env ::local not) (first args))]
                        [[:output (peek res)] (into [:input] (pop res))])
                      (analyze-apply env form))
                    (if-some [v (resolve-var env op)]
                      (if (:node (meta v))
                        (analyze-apply env form)
                        (analyze-form env (apply v form (if (:js-globals env) env (:locals env)) args)))
                      (analyze-form env (desugar-host env form)))))
                (analyze-apply env form)))
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
                (analyze env form) [[:literal form]]))
            (sorted-nodes [nodes]
              (->> nodes
                (vals)
                (sort-by :slot)
                (mapv :inst)))]
      (fn [env form]
        (let [[result nodes]
              (u/with-local nodes {}
                (-> env
                  (normalize-env)
                  (assoc ::local true)
                  (analyze-form form)))
              [local remote] (map (comp sorted-nodes nodes) [true false])]
          [(-> []
             (into (map peek) local)
             (into (mapcat pop) remote)
             (conj (peek result)))
           (-> []
             (into (mapcat pop) local)
             (into (map peek) remote)
             (into (pop result))
             (conj [:literal nil]))])))))

(tests

  (analyze {} '5) :=
  [[[:literal 5]]
   [[:literal nil]]]

  (analyze {} '(+ 2 3)) :=
  [[[:apply [:global :clojure.core/+] [:literal 2] [:literal 3]]]
   [[:literal nil]]]

  (analyze {} '(let [a 1] (+ a 2))) :=
  [[[:pub [:literal 1] [:apply [:global :clojure.core/+] [:sub 1] [:literal 2]]]]
   [[:literal nil]]]

  (analyze '{a nil} 'a) :=
  [[[:global :a]]
   [[:literal nil]]]

  (analyze {} '~m/none) :=
  [[[:variable [:global :missionary.core/none]]]
   [[:literal nil]]]

  (analyze {} '~@:foo) :=
  [[[:input]]
   [[:output [:literal :foo]] [:literal nil]]]

  (analyze {} '#':foo) :=
  [[[:constant [:literal :foo]]]
   [[:target] [:literal nil]]]

  (analyze {} '(do :a :b)) :=
  [[[:variable
     [:apply [:apply [:global :clojure.core/hash-map]]
      [:literal :a] [:constant [:literal :b]]]]]
   [[:target] [:literal nil]]]

  (analyze {} '(case 1 2 3 (4 5) ~@6 7)) :=
  [[[:variable
     [:pub [:constant [:literal 3]]
      [:pub [:constant [:input]]
       [:apply [:apply [:global :clojure.core/hash-map]
                [:literal 2] [:sub 2]
                [:literal 4] [:sub 1]
                [:literal 5] [:sub 1]]
        [:literal 1]
        [:constant [:literal 7]]]]]]]
   [[:target]
    [:target [:output [:literal 6]]]
    [:target]
    [:literal nil]]]

  (doto (def node) (alter-meta! assoc :macro true :node ()))
  (analyze {} 'node) :=
  [[[:literal nil] [:node 0]]
   [[:literal nil]]]

  (analyze {} '(def node)) :=
  [[[:literal nil] [:def 0]] [[:literal nil]]]
  )

(defn emit-log-failure [form]
  `(u/log-failure ~form))

(def emit
  (let [slots (u/local)
        init {:input 0 :target 0 :signal 0 :output 0 :constant 0}]
    (letfn [(slot [k]
              (let [m (u/get-local slots), n (get m k)]
                (u/set-local slots (assoc m k (inc n))) n))
            (emit-inst [sym pub [op & args]]
              (case op
                :sub (sym 'pub (- pub (first args)))
                :pub (list `let [(sym 'pub pub) (list `r/publish (sym 'context) (slot :signal)
                                                  (emit-inst sym pub (first args)))]
                       (emit-inst sym (inc pub) (second args)))
                :def (list `r/steady (cons `r/binder (cons (sym 'context) args)))
                :node (list `r/get-node (sym 'context) (first args))
                :apply (cons `m/latest (cons `u/call (mapv (partial emit-inst sym pub) args)))
                :input (cons `do (conj (mapv (partial emit-inst sym pub) args)
                                   (list `r/input (sym 'context) (slot :input))))
                :output (list `r/output (sym 'context) (slot :output) (emit-inst sym pub (first args)))
                :target (let [[forms {:keys [input target signal]}]
                              (u/with-local slots init (mapv (partial emit-inst sym pub) args))]
                          (list `r/target (sym 'context) (slot :target) input target signal
                            (cons `fn (cons [] forms))))
                :global (list `r/steady (symbol (first args)))
                :literal (list `r/steady (list `quote (first args)))
                :interop (cons `m/latest
                           (cons (let [arg-syms (into [] (map sym) (range (count (next args))))]
                                   (list `fn arg-syms (apply (first args) arg-syms)))
                             (mapv (partial emit-inst sym pub) (next args))))
                :constant (let [[form {:keys [input target signal]}]
                                (u/with-local slots init (emit-inst sym pub (first args)))]
                            (list `r/constant (sym 'context) (slot :constant) input target signal
                              (list `fn [] form)))
                :variable (list `r/variable (sym 'context) (emit-inst sym pub (first args)))))]
      (fn [sym insts]
        (let [[[forms] {:keys [input target signal]}]
              (u/with-local slots init
                (reduce
                  (fn [[forms insts] inst]
                    (let [insts (conj insts (emit-inst sym 0 inst))]
                      (case (first inst)
                        (:output :target) [forms insts]
                        [(conj forms (cons `do insts)) []])))
                  [[] []] insts))]
          (-> []
            (into (map-indexed
                    (fn [slot form]
                      (list `r/set-node (sym 'context)
                        slot (list `m/signal! form))))
              (pop forms))
            (conj (peek forms))
            (->>
              (cons [(sym 'context)])
              (cons `fn)
              (list `r/peer (dec (count forms)) input target signal))))))))

(tests
  (emit (comp symbol str)
    [[:literal 5]]) :=
  `(r/peer 0 0 0 0
     (fn [~'context]
       (do (r/steady '5))))

  (emit (comp symbol str)
    [[:apply [:global :clojure.core/+] [:literal 2] [:literal 3]]]) :=
  `(r/peer 0 0 0 0
     (fn [~'context]
       (do (m/latest u/call (r/steady +) (r/steady '2) (r/steady '3)))))

  (emit (comp symbol str)
    [[:pub [:literal 1]
      [:apply [:global :clojure.core/+]
       [:sub 1] [:literal 2]]]]) :=
  `(r/peer 0 0 0 1
     (fn [~'context]
       (do (let [~'pub0 (r/publish ~'context 0 (r/steady '1))]
             (m/latest u/call (r/steady +) ~'pub0 (r/steady '2))))))

  (emit (comp symbol str)
    [[:variable [:global :missionary.core/none]]]) :=
  `(r/peer 0 0 0 0
     (fn [~'context]
       (do (r/variable ~'context (r/steady m/none)))))

  (emit (comp symbol str)
    [[:input]]) :=
  `(r/peer 0 1 0 0
     (fn [~'context]
       (do (do (r/input ~'context 0)))))

  (emit (comp symbol str)
    [[:constant [:literal :foo]]]) :=
  `(r/peer 0 0 0 0
     (fn [~'context]
       (do (r/constant ~'context 0 0 0 0
             (fn [] (r/steady ':foo))))))

  (emit (comp symbol str)
    [[:variable
      [:pub [:constant [:literal 3]]
       [:pub [:constant [:input]]
        [:apply [:apply [:global :clojure.core/hash-map]
                 [:literal 2] [:sub 2]
                 [:literal 4] [:sub 1]
                 [:literal 5] [:sub 1]]
         [:literal 1]
         [:constant [:literal 7]]]]]]]) :=
  `(r/peer 0 0 0 2
     (fn [~'context]
       (do (r/variable ~'context
             (let [~'pub0 (r/publish ~'context 0
                            (r/constant ~'context 0 0 0 0
                              (fn [] (r/steady '3))))]
               (let [~'pub1 (r/publish ~'context 1
                              (r/constant ~'context 1 1 0 0
                                (fn [] (do (r/input ~'context 0)))))]
                 (m/latest u/call
                   (m/latest u/call (r/steady hash-map)
                     (r/steady '2) ~'pub0
                     (r/steady '4) ~'pub1
                     (r/steady '5) ~'pub1)
                   (r/steady '1)
                   (r/constant ~'context 2 0 0 0
                     (fn [] (r/steady '7))))))))))

  (emit (comp symbol str) [[:def 0]]) :=
  `(r/peer 0 0 0 0
     (fn [~'context]
       (do (r/steady (r/binder ~'context 0)))))

  )

(def args
  (map (comp symbol
         (partial intern *ns*)
         (fn [i]
           (with-meta (symbol (str "%" i))
             {:macro true :node ()})))
    (range)))

(defn resolve-runtime-throwing [env form]
  (when-not (symbol? form) (throw (ex-info "Symbol expected." {:form form})))
  (doto (resolve-runtime env form)
    (-> (when-not (throw (ex-info "Unable to resolve symbol." {:form form}))))))

(defn vars [env forms]
  (into {} (map (comp (juxt global identity)
                  (partial resolve-runtime-throwing
                    (normalize-env env)))) forms))