(ns hfdl.lang
  (:require [hfdl.impl.compiler :as c]
            [hfdl.impl.runtime :as r]))

(defmacro dataflow
  "Defines a dataflow program from given HFDL expressions, in an implicit `do`."
  [& body]
  (c/dataflow &env (cons `do body)))

(defn debug!
  "Runs given dataflow program in debug mode and returns a process instance."
  [program] (r/debug! program))

(comment
  (require '[missionary.core :as m])
  (def !input (atom "a"))
  (def >input (m/watch !input))
  (def child (dataflow (str @>input @>input)))
  (def parent (dataflow [@child]))
  (def p (debug! parent))
  @p
  (reset! !input "b")

 )