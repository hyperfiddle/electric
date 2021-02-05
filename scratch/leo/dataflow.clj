(ns leo.dataflow
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.analyzer.jvm :as clj]
            [cljs.analyzer.api :as cljs]
            [minitest :refer [tests]])
  (:import (clojure.lang Compiler$LocalBinding)))

(defn <- [flow] (throw (ex-info "Can't call <- outside of dataflow." {:flow flow})))

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

(def normalize-ast
  (letfn [(walk [env {:keys [op] :as ast}]
            (case op
              (:const)
              [:input (:val ast)]

              (:var)
              [:input (symbol (:var ast))]

              (:static-call)
              (into [:static (symbol (.getName ^Class (:class ast))
                               (name (:method ast)))]
                (map (partial walk env)) (:args ast))

              (:invoke)
              (into [:apply] (map (partial walk env))
                (cons (:fn ast) (:args ast)))

              (:do)
              (walk env (:ret ast))                         ;; fantasyland, discard statements

              (:let)
              (walk (reduce (fn [env {:keys [name init]}]
                              (assoc env name (walk env init)))
                      env (:bindings ast)) (:body ast))

              (:local)
              (or (env (:name ast)) [:input (:name ast)])

              (:if)
              [:if
               (walk env (:test ast))
               (walk env (:then ast))
               (walk env (:else ast))]

              ))]
    (partial walk {})))

(defn emit [prefix graph]
  graph)

(defn compile [main env prefix]
  (->> main
    (analyze-clj env)
    (normalize-ast)
    (emit prefix)))

(defmacro dataflow [main]
  (compile main &env (gensym "df")))

(tests
  (macroexpand '(dataflow (+ 1 2))) :=
  [:static 'clojure.lang.Numbers/add [:input 1] [:input 2]]

  (macroexpand '(dataflow (let [a 1] (+ a a)))) :=
  [:static 'clojure.lang.Numbers/add [:input 1] [:input 1]]

  (def !input (atom 0))
  (require '[missionary.core :as m])
  (macroexpand '(dataflow (let [i (m/watch !input)]
                            (if (odd? i) (inc i) i)))) :=
  '[:if
    [:apply [:input clojure.core/odd?] [:apply [:input missionary.core/watch] [:input leo.dataflow/!input]]]
    [:static clojure.lang.Numbers/inc [:apply [:input missionary.core/watch] [:input leo.dataflow/!input]]]
    [:apply [:input missionary.core/watch] [:input leo.dataflow/!input]]]

  )

(comment
  (def !input (atom 0))
  (def !input2 (atom 0))

  (deflow f [x]
    (if (even? (<- x))
      (inc (<- x)) 0))

  (dataflow
    (let [i (<- (m/watch !input))]
      [(if (odd? i) (inc i) ((fn [] i)))
       (* (f (m/watch !input2)) 2)]))

  )