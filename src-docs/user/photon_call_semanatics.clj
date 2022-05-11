(ns user.photon-call-semanatics
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]))

(p/def trace! println)

(p/defn Foo [n]
  (let [x (trace! (+ n 2))]      ; this is computed once, as you'd expect
    (p/fn [y]
      (+ y x))))

(p/defn Bar [c n]
  (let [F (Foo. c)]
    (p/for [x (range n)]
      (F. x))))

(tests
  (def !n (atom 3))
  (with (p/run
          (binding [trace! !]
            (! (Bar. 5 (p/watch !n)))))
    % := 7
    % := [7 8 9]
    (swap! !n inc)
    % := [7 8 9 10]))
