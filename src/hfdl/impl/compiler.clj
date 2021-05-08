(ns hfdl.impl.compiler
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.utils :as utils]
            [cljs.analyzer]
            [minitest :refer [tests]]
            [missionary.core :as m]
            [hfdl.impl.util :as u]
            [hfdl.impl.runtime :refer [->Dataflow]])
  (:import clojure.lang.Compiler$LocalBinding))

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
        (binding [cljs.env/*compiler* (or cljs.env/*compiler* (cljs.env/default-compiler-env))]
          (cljs.analyzer/analyze env form nil nil))
        (binding [clj/run-passes clj/scheduled-default-passes]
          (clj/analyze form
            (->> env
              (scope-bindings)
              (update (clj/empty-env) :locals merge))
            {:bindings {#'ana/macroexpand-1 macroexpand-1'}}))))))

(defn var-symbol [ast]
  (case (:op ast) :var (or (:name ast) (symbol (:var ast))) nil))

(defn global [s]
  (keyword
    (let [n (namespace s)]
      (case n "cljs.core" "clojure.core" n))
    (name s)))

(declare normalize-ast)

(defn deps [[op & args]]
  (case op
    :apply (into #{(first args)} (second args))
    (:remote :constant :variable) #{(first args)}
    (:local :global) #{}))

(def graphs (u/monoid u/map-into [#{} #{}]))

(defn build-graphs [graphs node]
  (if (contains? (first graphs) node)
    graphs (let [[op & args] node]
             (update (case op
                       :remote (-> graphs u/swap (build-graphs (second node)) u/swap)
                       (case op
                         :apply (reduce build-graphs (build-graphs graphs (first args)) (second args))
                         (:constant :variable) (build-graphs graphs (first args))
                         (:local :global) graphs))
               0 conj node))))

(defn result [node]
  {:result node :graphs (build-graphs (graphs) node)})

(defn discard [x & ys]
  (->> ys
    (map :graphs)
    (apply update x :graphs graphs)))

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
  [:global (global s)])

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
    (result (node-global (var-symbol ast)))

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
      (case (var-symbol fn)
        (clojure.core/unquote-splicing cljs.core/unquote-splicing)
        (let [df (normalize-ast (u/swap env) (first args))]
          (discard (result (node-remote (:result df))) (update df :graphs u/swap)))

        (clojure.core/unquote cljs.core/unquote)
        (normalize-par env node-constant (first args))

        (clojure.core/deref cljs.core/deref)
        (normalize-par env node-variable (first args))

        (apply normalize-par env node-apply fn args)))

    (:do)
    (apply discard (normalize-ast env (:ret ast))
      (map (partial normalize-ast env) (:statements ast)))

    (:let)
    (let [b (reduce normalize-binding {:env env :graphs (graphs)} (:bindings ast))]
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

    (throw (ex-info "Unsupported form." (select-keys ast [:form])))))

(def tc
  (letfn [(walk-dep [graphs dep]
            (if (contains? (first graphs) dep)
              graphs (update (walk-node graphs dep) 0 conj dep)))
          (walk-node [graphs [op & args]]
            (case op
              :remote (-> graphs u/swap (walk-dep (first args)) u/swap)
              :apply (reduce walk-dep (walk-dep graphs (first args)) (second args))
              (:constant :variable) (walk-dep graphs (first args))
              (:local :global) graphs))]
    (fn [graphs [l r]]
      (reduce walk-node (u/swap (reduce walk-node (u/swap graphs) r)) l))))

(defn emit [{:keys [result graphs]}]
  (->> graphs
    (tc [#{result} #{}])
    (mapv u/outof graphs)
    (list `->Dataflow result)))

(defn df [env form]
  (->> form
    (analyze-clj env)
    (normalize-ast empty-env)
    (emit)))

(defn vars [env forms]
  (into {} (map (comp (juxt global identity) var-symbol (partial analyze-clj env))) forms))

(tests
  "Constants"
  (df {} 1) := (emit (result (node-local 1)))
  (df {} [1 2]) := (emit (result (node-local [1 2])))
  (df {} {:a 1}) := (emit (result (node-local {:a 1})))
  (df {} #{:foo :bar}) := (emit (result (node-local #{:bar :foo}))))

(tests
  "Locals"
  (def a 1)
  (df {} `a) := (emit (result (node-global `a))))

(tests
  "Variables"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} `@a) := (emit (result (node-variable (node-global `a)))))

(tests
  "Vectors"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} [1 `@a])
  :=
  (emit
    (result
      (node-apply
        (node-global `vector)
        (node-local 1)
        (node-variable (node-global `a))))))

(tests
  "Unification"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} `[@a @a])
  :=
  (emit
    (result
      (node-apply
        (node-global `vector)
        (node-variable (node-global `a))
        (node-variable (node-global `a))))))

(tests
  "Maps"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} {:k `@a})
  :=
  (emit
    (result
      (node-apply
        (node-global `hash-map)
        (node-local :k)
        (node-variable (node-global `a)))))

  (df {} {`@a :v})
  :=
  (emit
    (result
      (node-apply
        (node-global `hash-map)
        (node-variable (node-global `a))
        (node-local :v)))))

(tests
  "Sets"
  (def !a (atom 0))
  (def a (m/watch !a))
  (df {} #{1 `@a})
  :=
  (emit
    (result
      (node-apply
        (node-global `hash-set)
        (node-local 1)
        (node-variable (node-global `a))))))

(tests
  "Static calls"
  (df {} '(java.lang.Math/sqrt 4))
  :=
  (emit
    (result
      (node-apply
        (node-local '(clojure.core/fn [arg0] (java.lang.Math/sqrt arg0)))
        (node-local 4)))))

(tests
  "Invoke"
  (df {} `(str 1))
  :=
  (emit
    (result
      (node-apply
        (node-global `str)
        (node-local 1)))))

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
  (emit
    (result
      (node-apply
        (node-global `str)
        (node-apply
          (node-global `str)
          (node-local 1))))))

(tests
  "Do"
  (df {} '(do 1 2))
  :=
  (emit (result (node-local 2)))                            ; no effect on 1, skipped.

  (df {} '(do (println 1) 2))
  :=
  (emit (discard (result (node-local 2)) (result (node-apply (node-global `println) (node-local 1))))))

(tests
  "Let is transparent"
  (df {} '(let [a 1] a))
  :=
  (emit (result (node-local 1))))

(tests
  "Let is not sequential"
  (df {} '(let [a 1 b 2 c 3] c))
  :=
  (emit (discard (result (node-local 3)) (result (node-local 1)) (result (node-local 2)))))

(tests
  "Referential transparency unifies bindings"
  (df {} '(let [a 1
                c (str a)
                b (str a)]
            [c b]))
  :=
  (emit
    (result
      (node-apply
        (node-global `vector)
        (node-apply (node-global `str) (node-local 1))
        (node-apply (node-global `str) (node-local 1))))))

(tests
  "If"
  (df {} '(if (odd? 1) :odd :even))
  :=
  (emit
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
          (node-constant (node-local :odd)))))))

(tests
  "Case"
  (df {} '(case 1
            (1 3) :odd
            2 :even
            4 :even
            :default))
  :=
  (emit
    (result
      (node-variable
        (node-apply (node-global `get)
          (node-apply (node-global `hash-map)
            (node-local 1) (node-constant (node-local :odd))
            (node-local 2) (node-constant (node-local :even))
            (node-local 3) (node-constant (node-local :odd))
            (node-local 4) (node-constant (node-local :even)))
          (node-local 1)
          (node-constant (node-local :default)))))))

(tests
  "fn"
  (df {} '(fn [] 1))
  :=
  (emit
    (result
      (node-apply (node-local '(clojure.core/fn [] (fn* ([] 1)))))))

  (df {} '(let [a 1] (fn [] (let [f inc] (f a)))))
  :=
  (emit
    (result
      (node-apply (node-local '(clojure.core/fn [a] (fn* ([] (let [f inc] (f a))))))
        (node-local 1)))))

(tests
  (defn form-input [])
  (defn query [_])
  (defn render-table [_])
  (df {}
    '(let [needle @(form-input)
           results ~@@(query needle)]
       @(render-table results)))
  :=
  (emit
    (result
      (node-variable
        (node-apply (node-global `render-table)
          (node-remote
            (node-variable
              (node-apply (node-global `query)
                (node-remote
                  (node-variable
                    (node-apply (node-global `form-input))))))))))))
