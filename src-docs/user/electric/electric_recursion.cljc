(ns user.electric.electric-recursion
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :refer [tests tap % with]]))

(hyperfiddle.rcf/enable!)

; Electric compiler hasn't implemented recursion syntax yet but the primitives are in place
; so you can do it with explicit dynamic vars

(e/def Ping)                                                ; all Electric defs are dynamic
(e/def Pong)

(tests
  "recursion"
  (def !x (atom 0))
  (with (e/run
          (try
            (binding [Ping (e/fn [x] (case x 0 [:done] (cons x (Ping. (dec x)))))]
              (tap (Ping. (e/watch !x))))))
    % := [:done]
    (swap! !x inc)
    % := [1 :done]
    (swap! !x inc)
    % := [2 1 :done]
    ))

(tests
  "mutual recursion"
  (def !x (atom 1))
  (with (e/run
          (binding [Ping (e/fn [x] (case x 0 [:done] (cons x (Pong. (dec x)))))
                    Pong (e/fn [x] (Ping. x))]
            (tap (Ping. (e/watch !x)))))
    % := [1 :done]
    (swap! !x inc)
    % := [2 1 :done]))

(tests
  "reactive fibonacci"
  (e/def Fib)
  (def !x (atom 5))
  (with (e/run (binding [Fib (e/fn [n]
                               (case n
                                 0 0
                                 1 1
                                 (+ (Fib. (- n 2))       ; self recur
                                    (Fib. (- n 1)))))]
                 (tap (Fib. (e/watch !x)))))
    % := 5
    (swap! !x inc)
    ; reactive engine will reuse the topmost frame, it is still naive fib though
    % := 8))

; Todo: self-recursion and Clojure recur form
