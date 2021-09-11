(declare defnode)

(comment
  (defnode foo [xs]
    (js/console.log
      (for [x xs]
        (upper-case
          ~@(str (type (inc x)))))))

  (defnode foo [xs]
    (js/console.log
      (for [x xs]
        (-> x inc type str ~@ upper-case))))

  '(-> x inc type str unquote-splicing upper-case) :=
  '()


  '~@(-> x inc type str ~@(upper-case))
  (macroexpand-1 '(-> x inc type str ~@ upper-case))
  (macroexpand-1 '(-> x inc type str ~@(upper-case)))
  (macroexpand-1 '~@(-> x inc type str ~@(upper-case)))

  )



