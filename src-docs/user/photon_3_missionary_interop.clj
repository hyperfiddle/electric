(ns user.photon-3-missionary-interop
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]
            [missionary.core :as m]))


(tests
  "Missionary signals"
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
  )

; Photon programs compile down to Missionary signals and therefore Photon has native interop with Missionary primitives.

(tests
  "introduce foreign Missionary signal to Photon program"
  (def !x (atom 0))

  (with (p/run (!
                 (new                                       ; construct flow with (new)
                   (m/watch !x))))                          ; recipe for a flow derived from atom
    % := 0
    (swap! !x inc)
    % := 1))

; Weirdly, Photon flows are "constructed" with (new)
; Why? That's not important right now, there is a symmetry with OOP which we will explain later
; (new) can be thought of as "await" or monadic join, but for flows
