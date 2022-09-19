(ns user.missionary.missionary-flow
  (:require [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m])
  (:import (missionary Cancelled)))

(hyperfiddle.rcf/enable!)


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
  (def !it (>x (fn [] (tap ::notify))
               (fn [] (tap ::terminate))))
  % := ::notify                                             ; lazy flow is ready to be sampled
  @!it := 0                                                 ; sample
  (swap! !x inc)                                            ; trigger a change
  % := ::notify                                             ; flow is ready again
  @!it := 1                                                 ; sample
  (!it)                                                     ; terminate
  % := ::notify
  @!it thrown? Cancelled                                    ; watch has terminated with this error
  % := ::terminate)

(tests
  "Aside: {} is pronounced 'discard', this is a missionary idiom"
  ; {} is an arity-2 fn that discards the first parameter
  ({} 1 2) := 2)

; explain relieve
; if you don't relieve a discrete flow, sampling is determined by sampling rate
