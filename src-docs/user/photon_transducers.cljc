(ns user.photon-transducers
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]
            [missionary.core :as m]))


(hyperfiddle.rcf/enable!)

(tests
  "discard"
  ; {}, in a missionary context pronounced "discard",
  ; is an arity-2 fn that discards the first parameter
  ({} 1 2) := 2)

(tests
  "Shockingly, Photon closures are concretely missionary continuous flows"
  (def !n (atom 0))
  (with (p/run
          (let [n (p/watch !n)
                X (p/fn [x] x)                              ;
                ; p/fn actually is not primitive. p/fn macroexpands to continuous flows with parameters injected
                ; through dynamic scope. Therefore, F (a Photon closure) is concretely a missionary continuous flow,
                ; whose argv must be injected by dynamic bindings, which is done by the special form (new).
                X10 (->> X                                  ; closures are missionary continuous flows, amazingly
                         (m/eduction (map (partial * 10)))  ; flow transducer
                         (m/relieve {}))]
            (! (new X10 n))))                               ; monadic join or "reactive unquote"
    % := 0
    (swap! !n inc)
    % := 10
    (swap! !n identity)
    % := 10
    (swap! !n inc)
    % := 20))

(tests
  "Dedupe photon flow with transducer"
  (def !n (atom 0))
  (with (p/run
          (let [x (p/watch !n)]
            (! (->> (p/fn [] x)
                    (m/eduction (dedupe))
                    (m/relieve {})
                    (new)))))
    % := 0
    (swap! !n inc)
    % := 1
    (swap! !n identity)
    ; % := 1 -- skipped by dedupe
    (swap! !n inc)
    % := 2))
