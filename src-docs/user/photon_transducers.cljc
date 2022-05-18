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
  "Shockingly, Photon closures are concretely missionary continuous flows"
  (def !x (atom 0))
  (with (p/run
          (let [x (p/watch !x)
                X (p/fn [x] x)                              ; a -> m a
                ; p/fn actually is not primitive. p/fn macroexpands to continuous flows with parameters injected
                ; through dynamic scope. Therefore, F (a Photon closure) is concretely a missionary continuous flow,
                ; whose argv must be injected by dynamic bindings, which is done by the special form (new).
                X10 (->> X                                  ; closures are missionary continuous flows, amazingly
                         (m/eduction (map (partial * 10)))  ; flow transducer
                         (m/relieve {}))]
            (! (new X10 x))))                               ; "reactive unquote" or "await" (m a -> a)
    % := 0
    (swap! !x inc)
    % := 10
    (swap! !x identity)
    % := 10
    (swap! !x inc)
    % := 20))

(tests
  "Dedupe photon flow with transducer"
  (def !x (atom 0))
  (with (p/run
          (let [x (p/watch !x)]
            (! (->> (p/fn [] x)
                    (m/eduction (dedupe))
                    (m/relieve {})
                    (new)))))
    % := 0
    (swap! !x inc)
    % := 1
    (swap! !x identity)
    ; % := 1 -- skipped by dedupe
    (swap! !x inc)
    % := 2))
