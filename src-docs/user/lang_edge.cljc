(ns user.lang-edge
  "Photon language edge cases"
  (:require [hfdl.lang :as r :refer [defnode node]]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

(tests
  (def !x (atom 0))
  (defnode f [x]
    (do (reset! !x (+ 1 x))    ; !
        (inc x)))
  (def dispose (r/run (f ~(m/watch !x))))
  ; infinite loop
  % := ::rcf/timeout
  (dispose))
