(ns dustin.dance1
  (:require
    [dance.core :refer [dance]]
    [delexical.core :refer [defdelexical]]
    [shuriken.core :refer [deconstruct disentangle unwrap-form]]
    [minitest :refer [tests]]))

(tests
  (dance
    '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
    :walk? #(and (seq? %) (-> % first (not= 'dec)))
    :pre? #(and (seq? %) (-> % first (not= '/)))
    :pre vec
    :post? number?
    :post (fn [x ctx] [(inc x) (update ctx :cnt inc)])
    :context {:cnt 0}
    :return :form-and-context
    :debug true)
  := '([+ 2 [- 3 [* 4 (/ 5 6) [dec 3]]]]
       {:debug-depth -1, :cnt 5, :scoped #{:scoped :depth}, :depth 0})

  )