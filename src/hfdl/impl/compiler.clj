(ns hfdl.impl.compiler
  (:require [cljs.analyzer.api :as cljs]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.utils :as utils]
            [minitest :refer [tests]]
            [missionary.core :as m]
            [hfdl.impl.util :as u])
  (:import clojure.lang.Compiler$LocalBinding
           (clojure.lang IFn IDeref)))

(def context (ThreadLocal.))

(defmacro get-ctx []
  `(.get ~(with-meta `context {:tag `ThreadLocal})))

(defmacro set-ctx [c]
  `(.set ~(with-meta `context {:tag `ThreadLocal}) ~c))

(defmacro with-ctx [ctx & body]
  `(let [ctx# (get-ctx)]
     (set-ctx ~ctx)
     (try ~@body (finally (set-ctx ctx#)))))

(defn bind-context [ctx flow]
  (fn [n t]
    (let [it (flow n t)]
      (reify
        IFn
        (invoke [_] (it))
        IDeref
        (deref [_] (with-ctx ctx @it))))))

(defn failer [n t e]
  (n) (reify
        IFn (invoke [_])
        IDeref (deref [_] (t) (throw e))))

(defn no-context [d n t]
  (failer n t (ex-info "Unable to find dafaflow context" {:dataflow d})))

(defrecord Dataflow [result graph]
  IFn (invoke [d n t] ((or (get-ctx) no-context) d n t)))

;; GG: This is a copy/paste from `clojure.tools.analyser.jvm/macroexpand-1`, without inlining.
(defn macroexpand-1'
  "If form represents a macro form,returns its expansion, else returns form."
  ([form] (macroexpand-1' form (clj/empty-env)))
  ([form env]
   (env/ensure (clj/global-env)
               (cond
                 (seq? form)    (let [[op & _args] form]
                                  (if (clj/specials op)
                                    form
                                    (let [v      (utils/resolve-sym op env)
                                          m      (meta v)
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
                 :else          form))))

(def analyze-clj
  (let [scope-bindings
        (partial reduce-kv
          (fn [scope symbol binding]
            (assoc scope
              symbol (or (when (instance? Compiler$LocalBinding binding)
                           (let [binding ^Compiler$LocalBinding binding]
                             {:op   :local
                              :tag  (when (.hasJavaClass binding)
                                      (some-> binding (.getJavaClass)))
                              :form symbol
                              :name symbol}))
                       binding))) {})]
    (fn [env form]
      (if (:js-globals env)
        (cljs/analyze env form)
        (binding [clj/run-passes clj/scheduled-default-passes]
          (clj/analyze form
                       (->> env
                            (scope-bindings)
                            (update (clj/empty-env) :locals merge))
                       {:bindings {#'ana/macroexpand-1 macroexpand-1'}}))))))

(defn remote [_])

(defn special [ast]
  (when (= :var (:op ast))
    (case (symbol (:var ast))
      clojure.core/deref :variable
      clojure.core/unquote :constant
      hfdl.impl.compiler/remote :remote
      nil)))

(declare normalize-ast)

(defn deps [[op & args]]
  (case op
    :apply (cons (first args) (second args))
    (:remote :constant :variable) (list (first args))
    (:local :global) ()))

(def graphs (u/monoid u/map-into [#{} #{}]))

(defn result [node]
  (->Dataflow node
    ((fn walk [graph node]
       (if (contains? (first graph) node)
         graph (let [[op & args] node]
                 (update (case op
                           :remote (-> graph u/swap (walk (second node)) u/swap)
                           (case op
                             :apply (reduce walk (walk graph (first args)) (second args))
                             (:constant :variable) (walk graph (first args))
                             (:local :global) graph))
                   0 conj node))))
     (graphs) node)))

(defn discard [x & ys]
  (->> ys
    (map :graph)
    (apply update x :graph graphs)))

(defn normalize-binding [r {:keys [name init]}]
  (let [s (normalize-ast (:env r) init)]
    (-> r
      (update :env update 0 assoc name (:result s))
      (discard s))))

(defn normalize-par [env ctor & deps]
  (let [dags (map (partial normalize-ast env) deps)]
    (apply discard (result (apply ctor (map :result dags))) dags)))

(defn node-apply [f & args]
  [:apply f (vec args)])

(defn node-local [x]
  [:local x])

(defn node-global [s]
  [:global s])

(defn node-variable [n]
  [:variable n])

(defn node-constant [n]
  [:constant n])

(defn node-remote [n]
  [:remote n])

(defn node-if [test then else]
  (node-variable
    (node-apply
      (node-global `get)
      (node-apply
        (node-global `hash-map)
        (node-local nil) [:constant else]
        (node-local false) [:constant else])
      test [:constant then])))

(defn node-case [test default & pairs]
  (node-variable
    (node-apply
      (node-global `get)
      (->> pairs
        (partition 2)
        (mapcat (fn [[k v]] [k [:constant v]]))
        (apply node-apply (node-global `hash-map)))
      test [:constant default])))

(declare normalize-ast)

(defn litteral-as-function
  "Rewrite a literal to its equivalent function call form:
  - {:a 1}  => (hash-map :a 1)
  - [:a 1]  => (vector :a 1)
  - #{:a 1} => (hash-set :a 1)"
  [env {:keys [op] :as ast}]
  (case op
    :vector (apply normalize-par env (partial node-apply (node-global `vector))   (:items ast))
    :set    (apply normalize-par env (partial node-apply (node-global `hash-set)) (:items ast))
    :map    (apply normalize-par env (partial node-apply (node-global `hash-map)) (interleave (:keys ast) (:vals ast)))
    :with-meta (apply normalize-par env (partial node-apply (node-global `with-meta)) [(:expr ast) (:meta ast)])))

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
         (apply merge))
    ))

(defn free-variables [local methods]
  (->> methods
    (map (fn [{:keys [params body]}]
           (free (into {} (map (juxt :name :form))
                   (if local (cons local params) params)) body)))
    (apply merge)))

(def empty-env [{} {}])

(defn resolve-local [[l r] s]
  (if-some [n (l s)]
    n (if-some [n (r s)]
        [:remote n])))

(defn normalize-ast [env {:keys [op] :as ast}]
  (case op
    (:const)
    (result (node-local (:val ast)))

    (:quote)
    (result (node-local (:form ast)))

    (:local)
    (result (or (resolve-local env (:name ast)) (node-local (:name ast))))

    (:map :vector :set :with-meta)
    (litteral-as-function env ast)

    (:var)
    (result (node-global (symbol (:var ast))))

    (:static-call)
    (let [{:keys [class method args]} ast
          arg-syms (into [] (map (comp symbol (partial str "arg"))) (range (count args)))]
      (apply normalize-par env
        (partial node-apply
          (->> arg-syms
            (cons (symbol (.getName ^Class class) (name method)))
            (list `fn arg-syms)
            (node-local))) args))

    (:invoke)
    (let [{:keys [fn args]} ast]
      (case (special fn)
        :remote (let [df (normalize-ast (u/swap env) (first args))]
                  (discard (result (node-remote (:result df))) (update df :graph u/swap)))
        :constant (normalize-par env node-constant (first args))
        :variable (normalize-par env node-variable (first args))
        (apply normalize-par env node-apply fn args)))

    (:do)
    (apply discard (normalize-ast env (:ret ast))
      (map (partial normalize-ast env) (:statements ast)))

    (:let)
    (let [b (reduce normalize-binding {:env env :graph (graphs)} (:bindings ast))]
      (discard (normalize-ast (:env b) (:body ast)) b))

    (:if)
    (normalize-par env node-if (:test ast) (:then ast) (:else ast))

    (:case)
    (if (= :throw (get-in ast [:default :op]))
      (throw (new IllegalArgumentException "Please provide a default for `case`. The `throw` special form is not supported yet."))
      (apply normalize-par env node-case (:test ast)
             (:default ast)
             (interleave (:tests ast)
                         (:thens ast))))
    (:case-test)
    (normalize-ast env (:test ast))

    (:case-then)
    (normalize-ast env (:then ast))

    (:fn)
    (let [{:keys [form local methods]} ast
          n->s (reduce-kv (fn [m l s]
                            (if-some [n (resolve-local env l)]
                              (assoc m n s) m)) {} (free-variables local methods))]
      (result (apply node-apply (node-local `(fn [~@(vals n->s)] ~form)) (keys n->s))))

    ;; (:throw) ;; TODO
    ;; (:new) ;; TODO
    ))

(defn assoc-count [m k]
  (assoc m k (count m)))

(defn topsort [sort node]
  (if (contains? sort node)
    sort (assoc-count (reduce topsort sort (deps node)) node)))

(defn emit-frame [dag]
  (let [slots (reduce topsort {} (:graph dag))]
    (list `->Dataflow
      (->> slots
        (sort-by val)
        (mapv (comp (fn [[op & args]]
                      (cons op
                        (case op
                          :apply [(slots (first args)) (mapv slots (second args))]
                          (:remote :constant :variable) [(slots (first args))]
                          (:local :global) args))) key))) (slots (:result dag)))))

(defn df [env form]
  (->> form
    (analyze-clj env)
    (normalize-ast empty-env)))

(tests
  "Constants"
  (df {} 1) := (result (node-local 1))
  (df {} [1 2]) := (result (node-local [1 2]))
  (df {} {:a 1}) := (result (node-local {:a 1}))
  (df {} #{:foo :bar}) := (result (node-local #{:bar :foo})))

(tests
  "Locals"
  (def a 1)
  (df {} `a) := (result (node-global `a)))

(tests
  "Variables"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} `@a) := (result (node-variable (node-global `a))))

(tests
  "Vectors"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} [1 `@a])
  :=
  (result
    (node-apply
      (node-global `vector)
      (node-local 1)
      (node-variable (node-global `a)))))

(tests
  "Unification"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} `[@a @a])
  :=
  (result
    (node-apply
      (node-global `vector)
      (node-variable (node-global `a))
      (node-variable (node-global `a)))))

(tests
  "Maps"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} {:k `@a})
  :=
  (result
    (node-apply
      (node-global `hash-map)
      (node-local :k)
      (node-variable (node-global `a))))

  (df {} {`@a :v})
  :=
  (result
    (node-apply
      (node-global `hash-map)
      (node-variable (node-global `a))
      (node-local :v))))

(tests
  "Sets"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} #{1 `@a})
  :=
  (result
    (node-apply
      (node-global `hash-set)
      (node-local 1)
      (node-variable (node-global `a)))))

(tests
  "Static calls"
  (df {} '(java.lang.Math/sqrt 4))
  :=
  (result
    (node-apply
      (node-local '(clojure.core/fn [arg0] (java.lang.Math/sqrt arg0)))
      (node-local 4))))

(tests
  "Invoke"
  (df {} `(str 1))
  :=
  (result
    (node-apply
      (node-global `str)
      (node-local 1))))

;; Inlining is disabled for now
#_(tests
 "Invoke inlined function"
 (dataflow {} `(inc 1))
 :=
 '(hfdl.impl.compiler/->Dataflow
   [[:local 1]
    [:local (clojure.core/fn [arg0] (clojure.lang.Numbers/inc arg0))]
    [:apply 1 [0]]]
   2))

(tests
  "Composition"
  (df {} `(str (str 1)))
  :=
  (result
    (node-apply
      (node-global `str)
      (node-apply
        (node-global `str)
        (node-local 1)))))

(tests
  "Do"
  (df {} '(do 1 2))
  :=
  (result (node-local 2)) ; no effect on 1, skipped.

  (df {} '(do (println 1) 2))
  :=
  (discard (result (node-local 2)) (result (node-apply (node-global `println) (node-local 1)))))

(tests
  "Let is transparent"
  (df {} '(let [a 1] a))
  :=
  (result (node-local 1)))

(tests
  "Let is not sequential"
  (df {} '(let [a 1 b 2 c 3] c))
  :=
  (discard (result (node-local 3)) (result (node-local 1)) (result (node-local 2))))

(tests
  "Referential transparency unifies bindings"
  (df {} '(let [a 1
                c (str a)
                b (str a)]
            [c b]))
  :=
  (result
    (node-apply
      (node-global `vector)
      (node-apply (node-global `str) (node-local 1))
      (node-apply (node-global `str) (node-local 1)))))

(tests
  "If"
  (df {} '(if (odd? 1) :odd :even))
  :=
  (result
    (node-variable
      (node-apply
        (node-global `get)
        (node-apply
          (node-global `hash-map)
          (node-local nil)
          (node-constant (node-local :even))
          (node-local false)
          (node-constant (node-local :even)))
        (node-apply
          (node-global `odd?)
          (node-local 1))
        (node-constant (node-local :odd))))))

(tests
  "Case"
  (:result (df {} '(case 1
                     (1 3) :odd
                     2 :even
                     4 :even
                     :default)))
  :=
  (node-variable
    (node-apply (node-global `get)
      (node-apply (node-global `hash-map)
        (node-local 1) (node-constant (node-local :odd))
        (node-local 2) (node-constant (node-local :even))
        (node-local 3) (node-constant (node-local :odd))
        (node-local 4) (node-constant (node-local :even)))
      (node-local 1)
      (node-constant (node-local :default)))))

(tests
  "fn"
  (df {} '(fn [] 1))
  :=
  (result
    (node-apply (node-local '(clojure.core/fn [] (fn* ([] 1))))))

  (:result (df {} '(let [a 1] (fn [] (let [f inc] (f a))))))
  :=
  (node-apply (node-local '(clojure.core/fn [a] (fn* ([] (let [f inc] (f a))))))
    (node-local 1)))

(tests
  (defn form-input [])
  (defn query [_])
  (defn render-table [_])
  (:result (df {}
             '(let [needle @(form-input)
                    results (remote @(query needle))]
                @(render-table results))))
  :=
  (node-variable
    (node-apply (node-global `render-table)
      (node-remote
        (node-variable
          (node-apply (node-global `query)
            (node-remote
              (node-variable
                (node-apply (node-global `form-input))))))))))
