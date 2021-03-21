(ns leo.composition
  (:require [minitest :refer [tests]]
            [missionary.core :as m]
            [clojure.walk :refer [macroexpand-all]]))

(declare <-)

(defn foo [a b]
  (<- a))

(defn quoted [f]
  `(quote ~f))

(defmacro deflow [name args & body]
  ; takes block of code and defines a macro
  ; that will inline that block of code where the macro is called
  (->> body
    (map quoted)
    (cons (->> args
            (mapcat (juxt quoted identity))
            (cons `vector)))
    (cons (quoted `let))
    (cons `list)
    (list `defmacro name args)))

(tests
  (macroexpand-1 '(deflow df1 [a b] (+ (inc a) (inc b))))
  := '(clojure.core/defmacro df1 [a b]
        (clojure.core/list (quote clojure.core/let)
          (clojure.core/vector (quote a) a (quote b) b)
          (quote (+ (inc a) (inc b))))))

(defn foo1   [a b] (+ (inc a) (inc b)))                       ; function call
(deflow foo2 [a b] (+ (inc a) (inc b)))                     ; macro expansion

(tests
  (foo1 10 100) := (foo2 10 100)

  (macroexpand-1 '(foo2 10 100))
  := '(let [a 10 b 100] (+ (inc a) (inc b)))
  (eval *1)


  '(clojure.core/defmacro df1 [a b]
     (clojure.core/list (quote clojure.core/let)
       (clojure.core/vector (quote a) a (quote b) b)
       (quote (+ (inc a) (inc b)))))

  (eval '(let [a 10 b 100]
           (clojure.core/list (quote clojure.core/let)
             (clojure.core/vector (quote a) a (quote b) b)
             (quote (+ (inc a) (inc b))))))
  := (let [a 10 b 100] (+ (inc a) (inc b)))

  )

(defn   bar1 [>input] [:input (<- >input)])
(deflow bar2 [>input] [:input (<- >input)])

(tests
  (bar1 1) := (bar1 1)
  (macroexpand-1 '(bar2 1))
  := '(clojure.core/let [>input 1] [:input (<- >input)])


  )

; Main limitation is it has to be top level
; can't put a deflow in the middle of clojure code

(map (fn [x]) (range 10))
(map (deflow [x]) (range 10))

(let [a (fn [x])])
(let [a (fn2 [x])])

reagent.core/partial



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
