(ns user.lang-edge
  "Photon language edge cases"
  (:require [hfdl.lang :as r :refer [defnode node]]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

(tests
  (def !x (atom 0))
  (def dispose (r/run (reset !x (! (inc ~(m/watch !x))))))
  ; infinite loop on construction, hang RCF. (while true)
  (dispose))
