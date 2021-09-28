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

(defn conj-res
  ([x] x)
  ([x y]
   (conj (into (pop x) (pop y))
     (conj (peek x) (peek y)))))

(defn void [cont]
  (fn rec [insts]
    (if-some [[inst & insts] (seq insts)]
      (conj inst (rec insts)) cont)))

(def analyze
  (let [nodes (ThreadLocal.)]
    (letfn [(visit-node [env sym form]                      ;; TODO detect cycles
              (let [l (::local env)]
                (if-some [node (-> (.get nodes) (get l) (get sym))]
                  (:slot node)
                  (let [sym-ns (symbol (namespace sym))
                        inst (-> env
                               (assoc :locals {} :ns (if (:js-globals env) (cljs/get-namespace sym-ns) sym-ns))
                               (dissoc ::index)
                               (analyze-form form))
                        slot (-> (.get nodes) (get l) count)]
                    (.set nodes (update (.get nodes) l update sym assoc :inst inst :slot slot :rank
                                  (+ slot (-> (.get nodes) (get (not l)) count)))) slot))))
            (resolve-node [env sym]
              (if-some [v (resolve-var env sym)]
                (let [m (meta v)
                      s (symbol (name (ns-name (:ns m))) (name (:name m)))]
                  (if-some [n (:node m)]
                    (visit-node env s n)
                    (throw (ex-info (str "Not a reactive var - " s) {}))))
                (throw (ex-info (str "Unable to resolve symbol - " sym) {}))))
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
                        (throw (ex-info "Can't take value of macro." {:symbol s}))))
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
                      (analyze-sexpr env (list {} x (cons `do xs))))
                    [[:literal nil]])

                  (if)
                  (let [[test then else] args]
                    (analyze-form env `(case ~test (nil false) ~else ~then)))

                  (case clojure.core/case cljs.core/case)
                  (analyze-form env
                    (let [clauses (vec (next args))
                          total? (odd? (count clauses))
                          partition (partition-all 2 (if total? (pop clauses) clauses))
                          symbols (repeatedly gensym)]
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
                                   ::cljs/call (fn [target & args] `(. ~target ~(:method dot) ~@args))
                                   ::cljs/access (fn [target] `(. ~target ~(symbol (str "-" (name (:field dot)))))))]]
                      (cons (:target dot) (:args dot))))

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

                          (if (:node m)
                            (analyze-apply env form)
                            (analyze-form env (apply v form (if (:js-globals env) env (:locals env)) args)))))
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
                (analyze env form) [[:literal form]]))]
      (fn [env form]
        (let [[inst nodes]
              (u/with-local nodes {}
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
                   nodes)) [true false]))))))

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

  (doto (def foo) (alter-meta! assoc :macro true :node '(do)))
  (doto (def bar) (alter-meta! assoc :macro true :node '(do foo)))
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

  (doto (def baz) (alter-meta! assoc :macro true :node '(do #'~@foo)))
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
     [:target [:nop] [:source [:literal nil]]]]]])

(defn emit-log-failure [form]
  `(u/log-failure ~form))

(def emit
  (let [slots (u/local)
        init {:locals (sorted-set)
              :nodes 0
              :inputs 0
              :targets 0
              :sources 0
              :signals 0
              :outputs 0
              :constants 0
              :variables 0}]
    (letfn [(slot [k]
              (let [m (u/get-local slots), n (get m k)]
                (u/set-local slots (assoc m k (inc n))) n))
            (node [s]
              (u/set-local slots (update (u/get-local slots) :nodes max s)) s)
            (emit-frame [sym form signals free]
              (list `fn [(sym 'frame) (sym 'nodes)]
                (reduce-kv
                  (fn [form i j]
                    (list `let
                      [(sym 'pub j)
                       (list `r/publish
                         (sym 'pub j) (sym 'context)
                         (sym 'frame) (+ signals i))]
                      form)) form free)))
            (emit-inst [sym pub [op & args]]
              (case op
                :nop nil
                :sub (let [i (- pub (first args))]
                       (u/set-local slots (update (u/get-local slots) :locals conj i))
                       (sym 'pub i))
                :pub (list `let [(sym 'pub pub) (list `r/publish (emit-inst sym pub (first args))
                                                  (sym 'context) (sym 'frame) (slot :signals))]
                       (emit-inst sym (inc pub) (second args)))
                :def (cons `r/capture (mapv node args))
                :node (list `nth (sym 'nodes) (node (first args)))
                :bind (let [[slot inst cont] args]
                        (list `let [(sym 'nodes) (list `assoc (sym 'nodes) (node slot) (emit-inst sym pub inst))]
                          (emit-inst sym pub cont)))
                :apply (cons `m/latest (cons `u/call (mapv (partial emit-inst sym pub) args)))
                :input (list `r/input (sym 'context) (sym 'frame) (slot :inputs))
                :output (list `do
                          (list `r/output (emit-inst sym pub (first args)) (sym 'context) (sym 'frame) (slot :outputs))
                          (emit-inst sym pub (second args)))
                :target (let [[form {:keys [locals inputs targets sources signals]}]
                              (u/with-local slots init (emit-inst sym pub (first args)))
                              free (into [] (take-while (partial > pub)) locals)]
                          (list `do
                            (list `r/target inputs targets sources (+ signals (count free))
                              (emit-frame sym form signals free)
                              (sym 'context) (sym 'frame) (slot :targets))
                            (emit-inst sym pub (second args))))
                :source (list `do
                          (list `r/source (sym 'nodes) (sym 'context) (sym 'frame) (slot :sources))
                          (emit-inst sym pub (first args)))
                :global (list `r/steady (symbol (first args)))
                :literal (list `r/steady (list `quote (first args)))
                :interop (cons `m/latest
                           (cons (let [arg-syms (into [] (map sym) (range (count (next args))))]
                                   (list `fn arg-syms (apply (first args) arg-syms)))
                             (mapv (partial emit-inst sym pub) (next args))))
                :constant (let [[form {:keys [locals inputs targets sources signals]}]
                                (u/with-local slots init (emit-inst sym pub (first args)))
                                free (into [] (take-while (partial > pub)) locals)]
                            (list `r/constant inputs targets sources (+ signals (count free))
                              (emit-frame sym form signals free)
                              (sym 'context) (sym 'frame) (slot :constants)))
                :variable (list `r/publish
                            (list `r/variable
                              (emit-inst sym pub (first args))
                              (sym 'nodes) (sym 'frame) (slot :variables))
                            (sym 'context) (sym 'frame) (slot :signals))))]
      (fn [sym inst]
        (let [[form {:keys [nodes inputs targets sources signals]}]
              (u/with-local slots init (emit-inst sym 0 inst))]
          (->> form
            (list `let [(sym 'frame) 0 (sym 'nodes) (vec (repeat nodes nil))])
            (list `fn [(sym 'context)])
            (list `r/peer inputs targets sources signals)))))))

(tests
  (emit (comp symbol str) [:literal 5]) :=
  `(r/peer 0 0 0 0
     (fn [~'context]
       (let [~'frame 0 ~'nodes []]
         (r/steady '5))))

  (emit (comp symbol str)
    [:apply [:global :clojure.core/+] [:literal 2] [:literal 3]]) :=
  `(r/peer 0 0 0 0
     (fn [~'context]
       (let [~'frame 0 ~'nodes []]
         (m/latest u/call (r/steady +) (r/steady '2) (r/steady '3)))))

  (emit (comp symbol str)
    [:pub [:literal 1]
     [:apply [:global :clojure.core/+]
      [:sub 1] [:literal 2]]]) :=
  `(r/peer 0 0 0 1
     (fn [~'context]
       (let [~'frame 0 ~'nodes []]
         (let [~'pub0 (r/publish (r/steady '1) ~'context ~'frame 0)]
           (m/latest u/call (r/steady +) ~'pub0 (r/steady '2))))))

  (emit (comp symbol str)
    [:variable [:global :missionary.core/none]]) :=
  `(r/peer 0 0 0 1
     (fn [~'context]
       (let [~'frame 0 ~'nodes []]
         (r/publish (r/variable (r/steady m/none) ~'nodes ~'frame 0) ~'context ~'frame 0))))

  (emit (comp symbol str) [:input]) :=
  `(r/peer 1 0 0 0
     (fn [~'context]
       (let [~'frame 0 ~'nodes []]
         (r/input ~'context ~'frame 0))))

  (emit (comp symbol str) [:constant [:literal :foo]]) :=
  `(r/peer 0 0 0 0
     (fn [~'context]
       (let [~'frame 0 ~'nodes []]
         (r/constant 0 0 0 0 (fn [~'frame ~'nodes] (r/steady ':foo)) ~'context ~'frame 0))))

  (emit (comp symbol str)
    [:variable
     [:pub [:constant [:literal 3]]
      [:pub [:constant [:input]]
       [:apply [:apply [:global :clojure.core/hash-map]
                [:literal 2] [:sub 2]
                [:literal 4] [:sub 1]
                [:literal 5] [:sub 1]]
        [:literal 1]
        [:constant [:literal 7]]]]]]) :=
  `(r/peer 0 0 0 3
     (fn [~'context]
       (let [~'frame 0 ~'nodes []]
         (r/publish
           (r/variable
             (let [~'pub0 (r/publish
                            (r/constant 0 0 0 0
                              (fn [~'frame ~'nodes] (r/steady '3))
                              ~'context ~'frame 0)
                            ~'context ~'frame 0)]
               (let [~'pub1 (r/publish
                              (r/constant 1 0 0 0
                                (fn [~'frame ~'nodes] (r/input ~'context ~'frame 0))
                                ~'context ~'frame 1)
                              ~'context ~'frame 1)]
                 (m/latest u/call
                   (m/latest u/call (r/steady hash-map)
                     (r/steady '2) ~'pub0
                     (r/steady '4) ~'pub1
                     (r/steady '5) ~'pub1)
                   (r/steady '1)
                   (r/constant 0 0 0 0
                     (fn [~'frame ~'nodes] (r/steady '7))
                     ~'context ~'frame 2))))
             ~'nodes ~'frame 0)
           ~'context ~'frame 2))))

  (emit (comp symbol str) [:def 0]) :=
  `(r/peer 0 0 0 0
     (fn [~'context]
       (let [~'frame 0 ~'nodes []]
         (r/capture 0))))

  (emit (comp symbol str)
    [:pub [:literal nil]
     [:constant [:sub 1]]]) :=
  `(r/peer 0 0 0 1
     (fn [~'context]
       (let [~'frame 0 ~'nodes []]
         (let [~'pub0 (r/publish (r/steady 'nil) ~'context ~'frame 0)]
           (r/constant 0 0 0 1
             (fn [~'frame ~'nodes]
               (let [~'pub0 (r/publish ~'pub0 ~'context ~'frame 0)] ~'pub0))
             ~'context ~'frame 0))))))

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