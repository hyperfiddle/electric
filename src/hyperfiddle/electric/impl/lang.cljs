(ns hyperfiddle.electric.impl.lang
  (:require [hyperfiddle.electric.impl.runtime :as r])
  (:require-macros [hyperfiddle.electric.impl.lang]))

(def ^{::type ::node, :doc "for loop/recur impl"} rec)
(def ^{::type ::node, :doc "for runtime arity check"} %arity)
(def ^{::type ::node, :doc "for self-recur"} %closure)
(def ^{::type ::node, :doc "for try/catch"} exception)
(def ^{::type ::node, :doc "for case"} %case-test)
(def ^{::type ::node, :doc "In a `catch` block, bound by the runtime to the current stacktrace. An Electric stacktrace is an ExceptionInfo. Use `hyperfiddle.electric.debug/stack-trace` to get a string representation."}
  trace {:fn (fn [_frame _vars _env] (r/pure nil)),
         :get-used-nodes #(), :var-name `trace
         :noutput 0, :ninput 0, :nvariable 0, :nsource 0, :ntarget 0, :dynamic '[], :nconstant 0})

(def ^{::type ::node} %0)
(def ^{::type ::node} %1)
(def ^{::type ::node} %2)
(def ^{::type ::node} %3)
(def ^{::type ::node} %4)
(def ^{::type ::node} %5)
(def ^{::type ::node} %6)
(def ^{::type ::node} %7)
(def ^{::type ::node} %8)
(def ^{::type ::node} %9)
(def ^{::type ::node} %10)
(def ^{::type ::node} %11)
(def ^{::type ::node} %12)
(def ^{::type ::node} %13)
(def ^{::type ::node} %14)
(def ^{::type ::node} %15)
(def ^{::type ::node} %16)
(def ^{::type ::node} %17)
(def ^{::type ::node} %18)
(def ^{::type ::node} %19)

(defn electric-only [& args]
  (throw (ex-info "I'm an electric value and you called me outside of electric." {:args args})))
