(ns user.photon-transducers
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]
            [missionary.core :as m]))


(hyperfiddle.rcf/enable!)

(tests
  "{} is discard, this is a missionary idiom"
  ; {} is an arity-2 fn that discards the first parameter
  ({} 1 2) := 2)

(tests
  "Dedupe photon flow with transducer"
  ; Photon thunks compile directly to missionary continuous flows (!)
  ; and therefore can be used with missionary API.
  (def !x (atom 0))
  (with (p/run
          (let [x (p/watch !x)
                <x (p/fn [] x)                              ; Normally in Photon we would name a p/fn with capital X,
                >x' (m/eduction (dedupe) <x)                ; but it is also concretely a missionary flow (!)
                <x' (m/relieve {} >x')]                     ; fully dedupe eagerly
            (new <x')))))                                   ; rejoin

(tests
  "->> is a common idiom currently"
  (def !x (atom 0))
  (with (p/run
          (let [x (p/watch !x)]
            (! (->> (p/fn [] x)                             ; lift :: a -> m a
                    (m/eduction (dedupe))
                    (m/relieve {})
                    (new)))))                               ; join :: m a -> a
    % := 0
    (swap! !x inc)
    % := 1
    (swap! !x identity)
    ; % := 1 -- skipped by dedupe
    (swap! !x inc)
    % := 2))
