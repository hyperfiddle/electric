(ns user.electric.electric-call-semanatics
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :refer [tests tap % with]]))

(e/def trace! println)

(e/defn Foo [n]
  (let [x (trace! (+ n 2))]      ; this is computed once, as you'd expect
    (e/fn [y]
      (+ y x))))

(e/defn Bar [c n]
  (let [F (Foo. c)]
    (e/for [x (range n)]
      (F. x))))

(tests
  (def !n (atom 3))
  (with (e/run
          (binding [trace! tap]
            (tap (Bar. 5 (e/watch !n)))))
    % := 7
    % := [7 8 9]
    (swap! !n inc)
    % := [7 8 9 10]))
