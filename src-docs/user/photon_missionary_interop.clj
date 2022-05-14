(ns user.photon-missionary-interop
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]
            [missionary.core :as m])
  (:import (missionary Cancelled)))


(hyperfiddle.rcf/enable!)

(tests
  "introduce Missionary signal to Photon program"
  ; Photon programs compile down to Missionary signals and therefore Photon has native interop with Missionary primitives.
  (def !x (atom 0))
  (with (p/run (! (let [X (m/watch !x)]                     ; X is a recipe for a signal that is derived from the atom
                    (new X))))                                 ; construct actual signal instance from the recipe with (new)
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

(tests
  "What is a Missionary flow, concretely?"
  (def !x (atom 0))                                         ; atoms model variable inputs
  (def >x (m/watch !x))                                     ; "recipe" for a signal derived from atom

  ; a signal is a "continuous flow" in Missionary jargon
  ; signals (flows) are recipes, like Haskell IO actions.
  ; Like IO actions, they are pure function values (thunks)
  ; and do not perform side effects until you run them.
  (fn? >x) := true                                          ; thunk (implementation detail)
  ; The atom has not been subscribed to yet, because >x is a pure value

  ; Flow thunks concretely have the structure (fn [notify! terminate!] !iterator),
  ; see https://github.com/leonoel/flow#specification
  (def !it (>x (fn [] (! ::notify))
               (fn [] (! ::terminate))))
  % := ::notify                                             ; lazy flow is ready to be sampled
  @!it := 0                                                 ; sample
  (swap! !x inc)                                            ; trigger a change
  % := ::notify                                             ; flow is ready again
  @!it := 1                                                 ; sample
  (!it)                                                     ; terminate
  % := ::notify
  @!it thrown? Cancelled                                    ; watch has terminated with this error
  % := ::terminate)
