(ns user.photon-missionary-interop
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]
            [missionary.core :as m]))


(hyperfiddle.rcf/enable!)

(tests
  "introduce Missionary signal to Photon program"
  ; Photon programs compile down to Missionary signals and therefore Photon has native interop with Missionary primitives.
  (def !x (atom 0))
  (with (p/run (! (let [X (m/watch !x)]                     ; X is a recipe for a signal that is derived from the atom
                    (new X))))                              ; construct actual signal instance from the recipe with (new)
    % := 0
    (swap! !x inc)
    % := 1))

(tests "dataflow diamond"
  (def !x (atom 0))
  (with (p/run (let [X (m/watch !x)                         ; missionary flow recipes are like Haskell IO actions
                     x (new X)]                             ; construct flow recipe once
                 (! (+ x x))))
    % := 0
    (swap! !x inc)
    % := 2
    (swap! !x inc)
    % := 4))

(tests "broken dataflow diamond"
  (def !x (atom 0))
  (with (p/run (let [X (m/watch !x)]
                 (! (+ (new X) (new X)))))                  ; bad - two separate watch instances on the same atom
    % := 0
    (swap! !x inc)                                          ; each instance fires an event resulting in two propagation frames
    % := 1                                                  ; bad
    % := 2
    (swap! !x inc)
    % := 3                                                  ; bad
    % := 4))

; if you don't relieve a discrete flow, sampling is determined by sampling rate
; new only works on contoinous flows, they must have initial value otherwise photon crash
