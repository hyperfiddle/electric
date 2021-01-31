(ns leo.composition
  (:require [minitest :refer [tests]]
            [missionary.core :as m]
            [clojure.walk :refer [macroexpand-all]]))

(defn <- [_])

(defn quoted [f]
  `(quote ~f))

(defmacro deflow [name args & body]
  (->> body
    (map quoted)
    (cons (->> args
            (mapcat (juxt quoted identity))
            (cons `vector)))
    (cons (quoted `let))
    (cons `list)
    (list `defmacro name args)))

(deflow df1 [a b]
  (+ (inc a) (inc b)))

(deflow df2 [>input]
  [:input (<- >input)])

(tests
  (macroexpand '(df1 :a :b)) :=
  '(let* [a :a b :b] (+ (inc a) (inc b)))
  )

(defmacro reactor [& body])

(tests
  (def !atom (atom 0))

  (macroexpand-all
    '(df1 42 (second (df2 (m/watch !atom)))))
  :=
  '(let* [a 42 b (second (let* [>input (m/watch !atom)]
                           [:input (<- >input)]))]
     (+ (inc a) (inc b)))
  )
