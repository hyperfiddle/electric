(ns dustin.dance1
  #_(:require
    [dorothy.core]
    [ubergraph.core]
    [dance.core :refer [dance]]
    [minitest :refer [tests]]))

(require '[dorothy.core])
(require '[ubergraph.core])
(require '[dance.core])
(require '[shuriken.core])
(require '[shuriken.sequential])

(comment
  (dance
    '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
    :walk? #(and (seq? %) (-> % first (not= 'dec)))
    :pre? #(and (seq? %) (-> % first (not= '/)))
    :pre vec
    :post? number?
    :post (fn [x ctx] [(inc x) (update ctx :cnt inc)])
    :context {:cnt 0}
    :return :form-and-context
    :debug true
    ))