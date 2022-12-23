(ns dustin.y2022.loop.fib
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests]]))

; https://en.wikipedia.org/wiki/Fibonacci_number (sequence)
; An integer sequence may be specified
;   explicitly by giving a formula for its nth term (aka "closed form solution"), or
;   implicitly by giving a relationship between its terms (aka "recurrence relation")
;
; explicit: f(x) = c^2 + 1            = 0, 3, 8, 15
;
; implicit: f(x) = f(x-1) + f(x-2)    = 0, 1, 1, 2, 3, 5, 8, 13
;           f(0) = 0; f(1) = 1
;
; Note that Fib is a recurrence relation, but also has a closed form solution.
; https://en.wikipedia.org/wiki/Fibonacci_number#Closed-form_expression
; https://en.wikipedia.org/wiki/Recurrence_relation

(defn fib [x]
  (case x
    0 1 1 1 ;1 1 2 1
    (+ (fib (- x 1))
       (fib (- x 2)))))

(tests
  (fib 0) := 1
  (fib 1) := 1
  (fib 2) := 2
  (fib 3) := 3
  (fib 4) := 5

  (map fib (range 5)) := [1 1 2 3 5])

(defn fib-iter [[a b]]
  (case b
    0 [1 1]
    [b (+' a b)]))

; todo loop recur

(tests
  (fib-iter [nil 0]) := [1 1]
  (fib-iter [1 1]) := [1 2]
  (fib-iter [1 2]) := [2 3]
  (fib-iter [2 3]) := [3 5]
  (fib-iter [3 5]) := [5 8])

(def fib2 (map first (iterate fib-iter [1 1])))

(tests
  (take 5 fib2) := [1 1 2 3 5])

(tests
  "is fib a reduction?"
  (reductions + (range 1 6)) := [1 3 6 10 15]

  ; No, because it's not naturally tail recursive?
  ;(reductions (fn [[acc v w :as buffer] x]
  ;              [(+' v w) w x])
  ;            [1 1 1]
  ;            (range 5))
  ; how to factor out the recursion given a buffer?
  ; Yes, you can iterate a matrix

  ; As it is equivalent for a sequence to satisfy a recurrence relation or to be the solution of a
  ; difference equation, the two terms "recurrence relation" and "difference equation" are
  ; sometimes used interchangeably.
  ;
  ; https://hf-inc.slack.com/archives/C0265L4HWDV/p1671388452606709
  )