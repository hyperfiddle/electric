(ns hfdl.lang
  (:require [hfdl.impl.compiler :as c]
            [hfdl.impl.runtime :as r]))

;;;;;;;;;;;;;;;;;;;
;; SPECIAL FORMS ;;
;;;;;;;;;;;;;;;;;;;

(defn spawn "Runs given HFDL program as a nested frame and returns its successive values." [program])
(defn <| "Runs given continuous flow and returns its successive values." [flow])
(defn |> "Captures variability of given expression and returns the flow of its successive values." [expr])


;;;;;;;;;;;;;;;;;;
;; ENTRY POINTS ;;
;;;;;;;;;;;;;;;;;;

(defmacro dataflow
  "Defines a dataflow program from given HFDL expressions, in an implicit `do`."
  [& body]
  (c/dataflow &env (cons `do body)))

(defn debug!
  "Runs given dataflow program in debug mode and returns a process instance."
  [program] (r/debug! program))