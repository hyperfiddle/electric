(ns hfdl.lang
  (:require [hfdl.impl.compiler :as c]
            [hfdl.impl.runtime :as r]))

(defmacro dataflow
  "Defines a dataflow program from given HFDL expressions, in an implicit `do`."
  [& body]
  (c/dataflow &env (cons `do body)))

(defn dbg! ;; Follow the pr/print and  prn/println clojure.core idiom.
  "Runs given dataflow program in debug mode and returns a process instance.
  `dbg!` is for machines, use `debug!` for a human-friendly representation."
  ([program]      (dbg! program {}))
  ([program opts] (r/debug! program opts)))

(defn debug!
  "Runs given dataflow program in debug mode and returns a human-readable process
  instance."
  [program]
  (dbg! program {:source-mapped true}))

(defn heap-dump [process]
  (reduce merge (:log process)))

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