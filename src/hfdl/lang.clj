(ns hfdl.lang)


;;;;;;;;;;;;;;;;;;;
;; SPECIAL FORMS ;;
;;;;;;;;;;;;;;;;;;;

(defn spawn "Runs given HFDL program as a nested frame and returns its successive values." [program])
(defn <| "Runs given continuous flow and returns its successive values." [flow])
(defn |> "Captures variability of given expression and returns the flow of its successive values." [expr])


;;;;;;;;;;;;;;;;;
;; ENTRY POINT ;;
;;;;;;;;;;;;;;;;;

(defmacro dataflow
  "Defines a dataflow program from given HFDL expressions, in an implicit `do`."
  [& body]
  (comment TODO))