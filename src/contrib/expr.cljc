(ns contrib.expr
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn quoted? [form]
  (and (seq? form)
       (= 'quote (first form))))

(tests
  (quoted? '(inc 1)) := false
  (quoted? ''(inc 1)) := true)