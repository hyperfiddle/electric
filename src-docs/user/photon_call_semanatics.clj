(ns user.photon-call-semanatics
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]))

(p/defn Foo [n trace!]
  (let [x (trace! (+ n 2))]      ; this is computed once, as you'd expect
    (p/fn [y]
      (+ y x))))

(p/defn Bar [c n trace!]
  (let [F (Foo. c trace!)]
    (p/for [x (range n)]
      (F. x))))

(tests
  (def !n (atom 3))
  (with (p/run (! (Bar. 5 (p/Watch. !n) !)))
    % := 7
    % := [7 8 9]
    (swap! !n inc)
    % := [7 8 9 10]))
