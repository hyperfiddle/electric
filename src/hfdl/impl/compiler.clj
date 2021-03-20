(ns hfdl.impl.compiler
  (:require [cljs.analyzer.api :as cljs]
            [clojure.tools.analyzer.jvm :as clj]
            [clojure.set :as set])
  (:import (clojure.lang Compiler$LocalBinding)))

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
          (->> env
            (scope-bindings)
            (update (clj/empty-env) :locals merge)
            (clj/analyze form)))))))

(defn special [ast]
  (when (= :var (:op ast))
    (case (symbol (:var ast))
      clojure.core/deref :variable
      clojure.core/unquote :constant
      hfdl.lang/spawn :spawn
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

(defn normalize-ast [env {:keys [op] :as ast}]
  (case op
    (:const)
    {:result [:local (:val ast)]}

    (:local)
    {:result (env (:name ast) [:local (:name ast)])}

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
    (normalize-par env node-if (:test ast) (:then ast) (:else ast))))

(defn deps [[op & args]]
  (case op
    :apply (into #{(first args)} (second args))
    (:spawn :constant :variable) #{(first args)}
    (:local :global) #{}))

(defn topsort [sort node]
  (if (contains? sort node)
    sort (let [sort (reduce topsort sort (deps node))]
           (assoc sort node (count sort)))))

(defrecord Dataflow [graph result])

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