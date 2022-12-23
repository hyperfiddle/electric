(ns dustin.y2022.loop.fac
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests]]))

; https://en.wikipedia.org/wiki/Factorial

; recurrence relation
; n! = n(n-1)! for n > 0
; 0! = 1

(defn fac [x]
  (case x
    0 1
    1 1
    (*' x (fac (dec x)))))

(tests
  (fac 0) := 1
  (fac 1) := 1
  (fac 2) := 2
  (fac 3) := 6

  (map fac (range 1 6)) := [1 2 6 24 120])

; closed form
; n! = PI {i=1..n) i.

(tests
  (reductions * 1 []) := [1] ; because 0! is the empty product `(*)` which is 1
  (reductions * 1 [2]) := [1 2]
  (reductions * 1 [2 3]) := [1 2 6]
  (reductions * 1 (range 2 6)) := [1 2 6 24 120])

(defn fac-iter [[acc x]]
  (case x
    0 [1 1]
    [(* acc (inc x)) (inc x)]))

(tests
  (fac-iter [nil 0]) := [1 1]
  (fac-iter [1 1]) := [2 2]
  (fac-iter [2 2]) := [6 3]
  (fac-iter [6 3]) := [24 4]
  (fac-iter [24 4]) := [120 5])


(def fac2 (map first (iterate fac-iter [1 1])))

(tests
  (take 5 fac2) := [1 2 6 24 120])
