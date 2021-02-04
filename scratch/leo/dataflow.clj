(ns leo.dataflow
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.analyzer.jvm :as clj]
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
      (binding [clj/run-passes clj/scheduled-default-passes]
        (->> env
          (scope-bindings)
          (update (clj/empty-env) :locals merge)
          (clj/analyze form))))))

(def analyze-ast
  (letfn [(sexp [stack ast]
            (let [graph (walk (peek stack) ast)]
              (conj (pop stack) (-> graph count dec) graph)))
          (walk [graph {:keys [op] :as ast}]
            (case op
              (:const :var)
              (conj graph {:input (case op :const (:val ast) :var (:var ast))})

              (:static-call)
              (let [stack (reduce sexp [graph] (:args ast))]
                (conj (peek stack)
                  {:type :derived
                   :deps (pop stack)
                   :proc (str (.getName ^Class (:class ast))
                           "/" (name (:method ast)))}))

              (:invoke)
              (let [stack (reduce sexp [graph] (cons (:fn ast) (:args ast)))]
                (conj (peek stack)
                  {:type :derived
                   :deps (pop stack)}))

              ))]
    (partial walk [])))

(defn emit [prefix graph]
  graph)

(defn compile [main env prefix]
  (->> main
    (analyze-clj env)
    (analyze-ast)
    (emit prefix)))

(defmacro dataflow [main]
  (compile main &env (gensym "df")))

(tests
  (macroexpand '(dataflow (+ 1 2))) :=
  [{:input 1} {:input 2} {:type :derived, :proc "clojure.lang.Numbers/add", :deps [0 1]}]

  )