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

(def ^:macro node)
(def ^:macro bind)

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
  (let [p (::peer env)
        t (get (::index env) p 0)]
    (-> env
      (update ::index assoc p (inc t))
      (update :locals update sym assoc
        :name sym ::peer p ::index t))))

(defn analyze-global [sym]
  (case (namespace sym)
    "js"
    [:interop (constantly sym)]
    [:global (global sym)]))

(def analyze
  (let [nodes (ThreadLocal.)]
    (letfn [(result [env l & rs]
              (let [p (::peer env)]
                {p l (not p) (vec rs)}))
            (merge-results
              ([_ x] x)
              ([env x y]
               (let [p (::peer env)]
                 {p       (conj (get x p) (get y p))
                  (not p) (into (get x (not p)) (get y (not p)))})))
            (collapse [env k x]
              (let [p (::peer env)]
                {p (get x p) (not p) [(into [k] (get x (not p)))]}))
            (analyze-node [env methods]
              (transduce
                (map
                  (fn [[expr & syms]]
                    (let [arity (count syms)
                          args (repeatedly arity gensym)]
                      (if (some #{'&} syms)
                        (throw (ex-info "TODO - vararg support" {}))
                        (analyze-form (reduce with-local env args)
                          (list `let (vec (interleave syms args)) expr))))))
                (partial merge-results env)
                (result env [])
                methods))
            (visit-node [env sym decl]
              (let [id [(::peer env) sym]]
                (:slot (or (get (.get nodes) id)
                         (-> {:slot (count (get (.get nodes) id))}
                           (doto (->> (assoc (.get nodes) id) (.set nodes)))
                           (assoc :methods (-> env
                                             (assoc :locals {} :ns (symbol (namespace sym)))
                                             (dissoc ::index)
                                             (analyze-node decl)))
                           (doto (->> (assoc (.get nodes) id) (.set nodes))))))))
            (resolve-node [env sym]
              (doto (when-some [v (resolve-var env sym)]
                      (let [m (meta v)]
                        (when (contains? m :node)
                          (visit-node env
                            (symbol (name (ns-name (:ns m)))
                              (name (:name m))) (:node m)))))
                (when-not (throw (ex-info (str "Unable to resolve node - " sym) {})))))
            (analyze-local [env sym loc]
              (if-some [i (::index loc)]
                (let [p (::peer loc)
                      s [:sub (- (get (::index env) p) i)]]
                  (if (= p (::peer env))
                    (result env s)
                    (result env [:input] [:output s])))
                (result env [:literal sym])))
            (analyze-symbol [env sym]
              (if-some [loc (get (:locals env) sym)]
                (analyze-local env sym loc)
                (if-some [sym (resolve-runtime env sym)]
                  (result env (analyze-global sym))
                  ;; TODO (clj/desugar-symbol form env)
                  (throw (ex-info "Unable to resolve symbol." {:symbol sym})))))
            (analyze-sexpr [env [op & args :as form]]
              (if (symbol? op)
                (if-some [loc (get (:locals env) op)]
                  (transduce
                    (map (partial analyze-form env))
                    (partial merge-results env)
                    (merge-results env
                      (result env [:invoke])
                      (analyze-local env op loc)) args)

                  (case op
                    (throw try def fn* letfn* loop* recur set! ns ns* deftype* defrecord*)
                    (throw (ex-info "Unsupported operation." {:op op :args args}))

                    (let*)
                    ((fn rec [env bs]
                       (if-some [[s i & bs] bs]
                         (merge-results env
                           (merge-results env
                             (result env [:pub])
                             (analyze-form env i))
                           (rec (with-local env s) bs))
                         (analyze-sexpr env (cons `do (next args)))))
                     env (seq (first args)))

                    (do)
                    (if-some [[x & xs] args]
                      (case xs
                        nil (analyze-form env x)
                        (analyze-sexpr env (list `case x (cons `do xs))))
                      (result env [:literal nil]))

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
                                (apply merge
                                  (map (fn [p s] (zipmap (parse-clause (first p)) (repeat s)))
                                    partition symbols))
                                (first args))
                              (list `let (vec (interleave symbols
                                                (map (fn [p] (list `var (second p)))
                                                  partition))))
                              (list `unquote)))
                          (throw (ex-info "TODO partial case" {})))))

                    (quote)
                    (result env [:literal form])

                    (var)
                    (collapse env :target
                      (merge-results env
                        (result env [:constant])
                        (analyze-form env (first args))))

                    (js* new)
                    (transduce
                      (map (partial analyze-form env))
                      (partial merge-results env)
                      [:interop (partial list op (first args))]
                      (next args))

                    (.)
                    (let [dot (cljs/build-dot-form [(first args) (second args) (nnext args)])]
                      (transduce
                        (map (partial analyze-form env))
                        (partial merge-results env)
                        (result env
                          [:interop (case (:dot-action dot)
                                      ::cljs/call (fn [target & args] `(. ~target ~(:method dot) ~@args))
                                      ::cljs/access (fn [target] `(. ~target ~(symbol (str "-" (name (:field dot)))))))])
                        (cons (:target dot) (:args dot))))

                    (if-some [sym (resolve-runtime env op)]
                      (case sym
                        (clojure.core/unquote cljs.core/unquote)
                        (merge-results env
                          (result env [:variable])
                          (analyze-form env (first args)))

                        (clojure.core/unquote-splicing cljs.core/unquote-splicing)
                        (let [p (::peer env)]
                          (-> (analyze-form (assoc env ::peer (not p)) (first args))
                            (update p (partial into [:input]))
                            (update (not p) (comp vector (partial vector :output)))))

                        (transduce
                          (map (partial analyze-form env))
                          (partial merge-results env)
                          (result env [:apply (analyze-global sym)]) args))

                      (if-some [v (resolve-var env op)]
                        (let [m (meta v)
                              s (symbol (name (ns-name (:ns m))) (name (:name m)))]
                          (case s
                            hfdl.impl.compiler/node
                            (assert nil)

                            hfdl.impl.compiler/bind
                            (assert nil)

                            (if (contains? m :node)
                              (let [s (visit-node env s (:node m))]
                                (assert nil))
                              (analyze-form env (apply v form (if (:js-globals env) env (:locals env)) args)))))
                        (analyze-form env (desugar-host env form))))))

                (transduce
                  (map (partial analyze-form env))
                  (partial merge-results env)
                  (result env [:apply]) form)))
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
                (analyze env form) (result env [:literal form])))]
      (fn [env form]
        (let [prev (.get nodes)]
          (.set nodes nil)
          (try (let [res (-> env
                           (normalize-env)
                           (assoc ::peer true)
                           (analyze-form form))
                     nodes (->> (vals (.get nodes)) (sort-by :slot))]
                 [[[(get res true)]] [(get res false)]])
               (finally (.set nodes prev))))))))

(tests

  (analyze {} '5) :=
  [[[[:literal 5]]]
   [[]]]

  (analyze {} '(+ 2 3)) :=
  [[[[:apply [:global :clojure.core/+] [:literal 2] [:literal 3]]]]
   [[]]]

  (analyze {} '(let [a 1] (+ a 2))) :=
  [[[[:pub [:literal 1] [:apply [:global :clojure.core/+] [:sub 1] [:literal 2]]]]]
   [[]]]

  (analyze {} '~m/none) :=
  [[[[:variable [:global :missionary.core/none]]]]
   [[]]]

  (analyze {} '~@:foo) :=
  [[[[:input]]]
   [[[:output [:literal :foo]]]]]

  (analyze {} '#':foo) :=
  [[[[:constant [:literal :foo]]]]
   [[[:target]]]]

  (analyze {} '(case 1 2 3 (4 5) ~@6 7)) :=
  [[[[:variable
      [:pub [:constant [:literal 3]]
       [:pub [:constant [:input]]
        [:apply [:apply [:global :clojure.core/hash-map]
                 [:literal 2] [:sub 2]
                 [:literal 4] [:sub 1]
                 [:literal 5] [:sub 1]]
         [:literal 1]
         [:constant [:literal 7]]]]]]]]
   [[[:target]
     [:target [:output [:literal 6]]]
     [:target]]]])

(defn emit-log-failure [form]
  `(u/log-failure ~form))

(def emit
  (let [slots (u/local)
        init {:input 0 :target 0 :signal 0 :output 0 :constant 0}]
    (letfn [(slot [k]
              (let [m (u/get-local slots), n (get m k)]
                (u/set-local slots (assoc m k (inc n))) n))
            (emit-inst [sym idx [op & args]]
              (case op
                :sub (sym 'idx (- idx (first args)))
                :pub (list `let [(sym 'idx idx)
                                 (list `aset (sym 'signal) (slot :signal)
                                   (list `m/signal! (emit-inst sym idx (first args))))]
                       (emit-inst sym (inc idx) (second args)))
                :apply (cons `m/latest (cons `u/call (mapv (partial emit-inst sym idx) args)))
                :input (cons `do (conj (mapv (partial emit-inst sym idx) args)
                                   (list `r/input (list `partial `aset (sym 'input) (slot :input)))))
                :output (list `r/output (sym 'context) (sym 'frame) (slot :output)
                          (emit-inst sym idx (first args)))
                :target (list `do
                          (list `aset (sym 'target) (slot :target)
                            (list `fn [(sym 'frame)] (emit-frame sym idx args)))
                          nil)
                :global (list `r/steady (symbol (first args)))
                :literal (list `r/steady (list `quote (first args)))
                :interop (cons `m/latest
                           (cons (let [arg-syms (into [] (map sym) (range (count (next args))))]
                                   (list `fn arg-syms (apply (first args) arg-syms)))
                             (mapv (partial emit-inst sym idx) (next args))))
                :constant (list `r/constant (sym 'context) (sym 'frame) (slot :constant)
                            (list `fn [(sym 'frame)] (emit-frame sym idx args)))
                :variable (list `s/switch (emit-inst sym idx (first args)))))
            (emit-frame [sym idx insts]
              (let [prev (u/get-local slots)]
                (u/set-local slots init)
                (try (let [results (mapv (partial emit-inst sym idx) insts)]
                       (->> results
                         (cons (cons `r/create (map sym '[context frame input target signal])))
                         (cons (reduce-kv
                                 (fn [b k n]
                                   (conj b (sym (name k))
                                     (list `object-array n)))
                                 [] (select-keys (u/get-local slots)
                                      [:input :target :signal])))
                         (cons `let)))
                     (finally (u/set-local slots prev)))))]
      (fn [nodes sym]
        (->> (emit-frame sym 0 (peek nodes))
          (list `let [(sym 'frame) 0])
          (list `fn [(sym 'context)]))))))

(tests
  (emit [[[:literal 5]]] (comp symbol str)) :=
  `(fn [~'context]
     (let [~'frame 0]
       (let [~'input (object-array 0)
             ~'target (object-array 0)
             ~'signal (object-array 0)]
         (r/create ~'context ~'frame ~'input ~'target ~'signal)
         (r/steady '5))))

  (emit [[[:apply [:global :clojure.core/+] [:literal 2] [:literal 3]]]]
    (comp symbol str)) :=
  `(fn [~'context]
     (let [~'frame 0]
       (let [~'input (object-array 0)
             ~'target (object-array 0)
             ~'signal (object-array 0)]
         (r/create ~'context ~'frame ~'input ~'target ~'signal)
         (m/latest u/call (r/steady +) (r/steady '2) (r/steady '3)))))

  (emit [[[:pub [:literal 1]
           [:apply [:global :clojure.core/+]
            [:sub 1] [:literal 2]]]]]
    (comp symbol str)) :=
  `(fn [~'context]
     (let [~'frame 0]
       (let [~'input (object-array 0)
             ~'target (object-array 0)
             ~'signal (object-array 1)]
         (r/create ~'context ~'frame ~'input ~'target ~'signal)
         (let [~'idx0 (aset ~'signal 0 (m/signal! (r/steady '1)))]
           (m/latest u/call (r/steady +) ~'idx0 (r/steady '2))))))

  (emit [[[:variable [:global :missionary.core/none]]]] (comp symbol str)) :=
  `(fn [~'context]
     (let [~'frame 0]
       (let [~'input (object-array 0)
             ~'target (object-array 0)
             ~'signal (object-array 0)]
         (r/create ~'context ~'frame ~'input ~'target ~'signal)
         (s/switch (r/steady m/none)))))

  (emit [[[:input]]] (comp symbol str)) :=
  `(fn [~'context]
     (let [~'frame 0]
       (let [~'input (object-array 1)
             ~'target (object-array 0)
             ~'signal (object-array 0)]
         (r/create ~'context ~'frame ~'input ~'target ~'signal)
         (do (hfdl.impl.runtime/input (partial aset ~'input 0))))))

  (emit [[[:constant [:literal :foo]]]] (comp symbol str)) :=
  `(fn [~'context]
     (let [~'frame 0]
       (let [~'input (object-array 0)
             ~'target (object-array 0)
             ~'signal (object-array 0)]
         (r/create ~'context ~'frame ~'input ~'target ~'signal)
         (r/constant ~'context ~'frame 0
           (fn [~'frame]
             (let [~'input (object-array 0)
                   ~'target (object-array 0)
                   ~'signal (object-array 0)]
               (r/create ~'context ~'frame ~'input ~'target ~'signal)
               (r/steady ':foo)))))))

  (emit [[[:variable
           [:pub [:constant [:literal 3]]
            [:pub [:constant [:input]]
             [:apply [:apply [:global :clojure.core/hash-map]
                      [:literal 2] [:sub 2]
                      [:literal 4] [:sub 1]
                      [:literal 5] [:sub 1]]
              [:literal 1]
              [:constant [:literal 7]]]]]]]] (comp symbol str)) :=
  `(fn [~'context]
     (let [~'frame 0]
       (let [~'input (object-array 0)
             ~'target (object-array 0)
             ~'signal (object-array 2)]
         (r/create ~'context ~'frame ~'input ~'target ~'signal)
         (s/switch
           (let [~'idx0 (aset ~'signal 0
                           (m/signal! (r/constant ~'context ~'frame 0
                                        (fn [~'frame]
                                          (let [~'input (object-array 0)
                                                ~'target (object-array 0)
                                                ~'signal (object-array 0)]
                                            (r/create ~'context ~'frame ~'input ~'target ~'signal)
                                            (r/steady '3))))))]
             (let [~'idx1 (aset ~'signal 1
                             (m/signal! (r/constant ~'context ~'frame 1
                                          (fn [~'frame]
                                            (let [~'input (object-array 1)
                                                  ~'target (object-array 0)
                                                  ~'signal (object-array 0)]
                                              (r/create ~'context ~'frame ~'input ~'target ~'signal)
                                              (do (r/input (partial aset ~'input 0))))))))]
               (m/latest u/call
                 (m/latest u/call (r/steady hash-map)
                   (r/steady '2) ~'idx0
                   (r/steady '4) ~'idx1
                   (r/steady '5) ~'idx1)
                 (r/steady '1)
                 (r/constant ~'context ~'frame 2
                   (fn [~'frame]
                     (let [~'input (object-array 0)
                           ~'target (object-array 0)
                           ~'signal (object-array 0)]
                       (r/create ~'context ~'frame ~'input ~'target ~'signal)
                       (r/steady '7))))))))))))

(defn resolve-runtime-throwing [env form]
  (when-not (symbol? form) (throw (ex-info "Symbol expected." {:form form})))
  (doto (resolve-runtime env form)
    (-> (when-not (throw (ex-info "Unable to resolve symbol." {:form form}))))))

(defn vars [env forms]
  (into {} (map (comp (juxt global identity)
                  (partial resolve-runtime-throwing
                    (normalize-env env)))) forms))