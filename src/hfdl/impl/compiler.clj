(ns hfdl.impl.compiler
  (:require [cljs.analyzer.api :as cljs]
            [clojure.set :as set]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.utils :as utils]
            [minitest :refer [tests]]
            [missionary.core :as m])
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

(defrecord Dataflow [graph result]
  IFn (invoke [d n t]
        (if-some [c (get-ctx)]
          (c d n t)
          (do (n)
              (reify
                IFn (invoke [_])
                IDeref (deref [_] (t)
                         (throw (ex-info "Unable to find dafaflow context"
                                  {:dataflow d}))))))))

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

(defn special [ast]
  (when (= :var (:op ast))
    (case (symbol (:var ast))
      clojure.core/deref :variable
      clojure.core/unquote :constant
      nil)))

(declare normalize-ast)

(defn ignore-result [{:keys [result effects]}]
  (set/union effects #{result}))

(defn normalize-binding [r {:keys [name init]}]
  (let [s (normalize-ast (:env r) init)]
    (-> r
      (update :env assoc name (:result s))
      (update :effects set/union (ignore-result s)))))

(defn normalize-par [env ctor & deps]
  (let [dags (map (partial normalize-ast env) deps)]
    {:result (apply ctor (map :result dags))
     :effects (apply set/union (map :effects dags))}))

(defn node-apply [f & args]
  [:apply f args])

(defn node-if [test then else]
  [:variable
   [:apply [:global `get]
    [[:apply [:global `hash-map]
      [[:local nil]   [:constant else]
       [:local false] [:constant else]]]
     test [:constant then]]]])

(defn node-case [test default & pairs]
  [:variable
   [:apply [:global `get]
    [[:apply [:global `hash-map]
      (->> pairs
           (partition 2)
           (mapcat (fn [[k v]] [k [:constant v]])))]
     test
     [:constant default]]]])

(declare normalize-ast)

(defn litteral-as-function
  "Rewrite a literal to its equivalent function call form:
  - {:a 1}  => (hash-map :a 1)
  - [:a 1]  => (vector :a 1)
  - #{:a 1} => (hash-set :a 1)"
  [env {:keys [op] :as ast}]
  (case op
    :vector (apply normalize-par env (partial node-apply [:global `vector])   (:items ast))
    :set    (apply normalize-par env (partial node-apply [:global `hash-set]) (:items ast))
    :map    (apply normalize-par env (partial node-apply [:global `hash-map]) (interleave (:keys ast) (:vals ast)))
    :with-meta (apply normalize-par env (partial node-apply [:global `with-meta]) [(:expr ast) (:meta ast)])))

(defn free [env ast]
  (case (:op ast)
    (:const :var :vector)
    {}

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

    ))

(defn free-variables [local methods]
  (->> methods
    (map (fn [{:keys [params body]}]
           (free (into {} (map (juxt :name :form))
                   (if local (cons local params) params)) body)))
    (apply merge)))

(defn normalize-ast [env {:keys [op] :as ast}]
  (case op
    (:const)
    {:result [:local (:val ast)]}

    (:quote)
    {:result [:local (:form ast)]}

    (:local)
    {:result (env (:name ast) [:local (:name ast)])}

    (:map :vector :set :with-meta)
    (litteral-as-function env ast)

    (:var)
    {:result [:global (symbol (:var ast))]}

    (:static-call)
    (let [{:keys [class method args]} ast
          arg-syms (into [] (map (comp symbol (partial str "arg"))) (range (count args)))]
      (apply normalize-par env
        (partial node-apply
          [:local (->> arg-syms
                    (cons (symbol (.getName ^Class class) (name method)))
                    (list `fn arg-syms))]) args))

    (:invoke)
    (if-some [special (special (:fn ast))]
      (apply normalize-par env (partial vector special) (:args ast))
      (apply normalize-par env node-apply (:fn ast) (:args ast)))

    (:do)
    (->> (:statements ast)
      (map (comp ignore-result (partial normalize-ast env)))
      (apply update (normalize-ast env (:ret ast)) :effects set/union))

    (:let)
    (let [{:keys [env effects]} (reduce normalize-binding {:env env} (:bindings ast))]
      (update (normalize-ast env (:body ast)) :effects set/union effects))

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
          syms (into [] (filter (comp env key)) (free-variables local methods))]
      {:result [:apply [:local `(fn ~(mapv val syms) ~form)]
                (into [] (map (comp env key)) syms)]})

    ;; (:throw) ;; TODO
    ;; (:new) ;; TODO
    ))

(defn deps [[op & args]]
  (case op
    :apply (into #{(first args)} (second args))
    (:spawn :constant :variable) #{(first args)}
    (:local :global) #{}))

(defn topsort [sort node]
  (if (contains? sort node)
    sort (let [sort (reduce topsort sort (deps node))]
           (assoc sort node (count sort)))))

(defn emit-frame [dag]
  (let [slots (reduce topsort {} (ignore-result dag))]
    (list `->Dataflow
      (->> slots
        (sort-by val)
        (mapv (comp (fn [inst]
                      (case (first inst)
                        :apply (-> inst
                                 (update 1 slots)
                                 (update 2 (partial mapv slots)))
                        (:spawn :constant :variable) (update inst 1 slots)
                        :global (update inst 1 (partial list `quote))
                        :local inst)) key))) (slots (:result dag)))))

(defn dataflow [env form]
  (->> form
    (analyze-clj env)
    (normalize-ast {})
    (emit-frame)))

(tests
 "Constants"
 (dataflow {} 1) := `(->Dataflow [[:local 1]] 0)
 (dataflow {} [1 2]) := `(->Dataflow [[:local [1 2]]] 0)
 (dataflow {} {:a 1}) := `(->Dataflow [[:local {:a 1}]] 0)
 (dataflow {} #{:foo :bar}) := `(->Dataflow [[:local #{:foo :bar}]] 0))

(tests
 "Locals"
 (def a 1)
 (dataflow {} `a) := `(->Dataflow [[:global `a]] 0))

(tests
 "Variables"
 (def !a (atom 0))
 (def a (m/watch !a))
 (dataflow {} `@a) := `(->Dataflow [[:global 'a] [:variable 0]] 1))

(tests
 "Vectors"
 (def !a (atom 0))
 (def a (m/watch !a))
 (dataflow {} [1 `@a])
 :=
 `(->Dataflow [[:global 'clojure.core/vector]
               [:global 'hfdl.impl.compiler/a]
               [:variable 1]
               [:local 1]
               [:apply 0 [3 2]]]
              4))

(tests
 "Unification"
 (def !a (atom 0))
 (def a (m/watch !a))
 (dataflow {} `[@a @a])
 :=
 `(->Dataflow [[:global 'clojure.core/vector]
               [:global 'hfdl.impl.compiler/a] ; single signal for a.
               [:variable 1]                   ; single deref for a.
               [:apply 0 [2 2]]]
              3))

(tests
 "Maps"
 (def !a (atom 0))
 (def a (m/watch !a))
 (dataflow {} {:k `@a})
 :=
 `(->Dataflow [[:global 'clojure.core/hash-map]
               [:global 'hfdl.impl.compiler/a]
               [:variable 1]
               [:local :k]
               [:apply 0 [3 2]]]
              4)

 (dataflow {} {`@a :v})
 :=
 `(->Dataflow [[:global 'clojure.core/hash-map]
               [:global 'hfdl.impl.compiler/a]
               [:variable 1]
               [:local :v]
               [:apply 0 [2 3]]]
              4))

(tests
 "Sets"
 (def !a (atom 0))
 (def a (m/watch !a))
 (dataflow {} #{1 `@a})
 :=
 `(->Dataflow [[:global 'clojure.core/hash-set]
               [:global 'hfdl.impl.compiler/a]
               [:variable 1]
               [:local 1]
               [:apply 0 [3 2]]]
                                        4))

(tests
 "Static calls"
 (dataflow {} '(java.lang.Math/sqrt 4))
 := '(hfdl.impl.compiler/->Dataflow
      [[:local 4]
       [:local (clojure.core/fn [arg0] (java.lang.Math/sqrt arg0))]
       [:apply 1 [0]]]
      2))

(tests
 "Invoke"
 (dataflow {} `(str 1))
 :=
 `(->Dataflow
   [[:local 1]
    [:global 'clojure.core/str]
    [:apply 1 [0]]]
   2))

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
 (dataflow {} `(str (str 1)))
 :=
 `(->Dataflow [[:local 1]
               [:global 'clojure.core/str] ; unified
               [:apply 1 [0]]
               [:apply 1 [2]]]
              3))

(tests
 "Do"
 (dataflow {} '(do 1 2)) := `(->Dataflow [[:local 2]] 0) ; no effect on 1, skipped.

 (dataflow {} '(do (println 1) 2))
 :=
 `(->Dataflow [[:local 1]                                ; effect on 1, retained.
               [:global 'clojure.core/println]
               [:apply 1 [0]]
               [:local 2]]
              3))

(tests
 "Let is transparent"
 (dataflow {} '(let [a 1] a)) := `(->Dataflow [[:local 1]] 0))

(tests
 "Let is not sequential"
 (dataflow {} '(let [a 1
                     b 2
                     c 3]
                 c))
 :=
 `(->Dataflow [[:local 3] [:local 1] [:local 2]] 0))

(tests
 "Referential transparency unifies bindings"
 (dataflow {} '(let [a 1
                     c (str a)
                     b (str a)]
                 [c b]))
 :=
 `(->Dataflow [[:local 1]
               [:global 'clojure.core/str]
               [:apply 1 [0]]
               [:global 'clojure.core/vector]
               [:apply 3 [2 2]]]
              4))

(tests
 "If"
 (dataflow {} '(if (odd? 1) :odd :even))
 :=
 `(->Dataflow
  [[:global 'clojure.core/hash-map]
   [:local false]
   [:local :even]
   [:constant 2]
   [:local nil]
   [:apply 0 [4 3 1 3]]
   [:global 'clojure.core/get]
   [:local :odd]
   [:constant 7]
   [:global 'clojure.core/odd?]
   [:local 1]
   [:apply 9 [10]]
   [:apply 6 [5 11 8]]
   [:variable 12]]
  13))

(tests
 "Case"
 (dataflow {} '(case 1
                 (1 3) :odd
                 2     :even
                 4     :even
                 :default))
 :=
 `(->Dataflow
   [[:global 'clojure.core/get]
    [:local 1]
    [:local :default] ; broken here, waiting for :default/throw handling
    [:constant 2]
    [:global 'clojure.core/hash-map]
    [:local :even]
    [:constant 5]
    [:local 3]
    [:local 4]
    [:local 2]
    [:local :odd]
    [:constant 10]
    [:apply 4 [1 11 9 6 7 11 8 6]]
    [:apply 0 [12 1 3]]
    [:variable 13]]
   14))

(comment
  "fn"
  (dataflow {} '(fn [] 1))
  :=
  `(->Dataflow
     [[:local (fn [] (fn* ([] 1)))]
      [:apply 0]]
     1)

  (dataflow {} '(let [a 1] (fn [] (inc a))))
  :=
  (->Dataflow
    [[:local (fn [a] (fn [] (let [f inc] (f a))))]
     [:local 1]
     [:apply 0 [1]]]
    2)

  )