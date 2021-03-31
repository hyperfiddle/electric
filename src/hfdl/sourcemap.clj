(ns hfdl.sourcemap
  (:require [hfdl.lang :refer [dataflow debug!]]
            [minitest :refer [tests]]
            [missionary.core :as m]))

(defn decompile [program]
  (reduce (fn [asts [op & args]]
            (conj asts
                  (case op
                    :apply           (cons (asts (first args)) (map asts (second args)))
                    :constant        (list `clojure.core/unquote (asts (first args)))
                    :variable        (list `clojure.core/deref (asts (first args)))
                    (:local :global) (first args))))
          []
          (:graph program)))

(tests
 (defmacro recover [ast]
   `(decompile (dataflow ~ast)))

 (declare a f)
 (recover a) := `[a]
 (recover @a) := `[a @a]
 (recover (f @a)) := `[a @a f (f @a)]
 (recover [(f @a) (f @a)]) := `[vector a @a f (f @a) (vector (f @a) (f @a))]
 )

(defn locate [program form]
  (->> (decompile program)
       (map-indexed vector)
       (filter (fn [[_idx form']] (= form form'))) ; .indexOf
       (ffirst)))

(tests
 (declare a f)
 (def program (dataflow (f (str a) (str a))))

 (locate program `a) := 0
 (locate program `str) := 1
 (locate program `(str a)) := 2
 (locate program `f) := 3
 (locate program `(f (str a) (str a))) := 4)

(defn heap-dump [process]
  (reduce merge (:log process)))

(defn humanize
  "Correlate a heap dump with the original program."
  [program heap-dump]
  (let [decompiled (decompile program)]
    (reduce-kv (fn [r k v]
                 (assoc r (get-in decompiled k) v))
               {}
               heap-dump)))

(tests
 (def !a (atom 0))
 (def a (m/watch !a))
 (def inc' inc) ; prevent inlining
 (def dec' dec)
 (def program (dataflow [(inc' @a) (dec' @a)]))
 (def process (debug! program))

 (heap-dump @process)  := {[0] #'clojure.core/vector,
                           [1] #'hfdl.sourcemap/a,
                           [2] 0,
                           [3] #'hfdl.sourcemap/inc',
                           [4] 1,
                           [5] #'hfdl.sourcemap/dec',
                           [6] -1,
                           [7] [1 -1]}

 ;; Won't pass if ns is re-evaluated because vars are resolved before
 ;; re-definition. Vars are problematic.
 (humanize program (heap-dump @process))
 :=
 {`vector                       #'clojure.core/vector,
  `a                            #'hfdl.sourcemap/a,
  `@a                           0,
  `inc'                         #'hfdl.sourcemap/inc',
  `(inc' @a)                    1,
  `dec'                         #'hfdl.sourcemap/dec',
  `(dec' @a)                    -1,
  `(vector (inc' @a) (dec' @a)) [1 -1]}
 )
