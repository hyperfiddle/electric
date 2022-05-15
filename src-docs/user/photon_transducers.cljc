(ns user.photon-transducers
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]
            [missionary.core :as m]))


(hyperfiddle.rcf/enable!)

(tests
  "Photon thunks are Missionary flows! and therefore inherit transducers from Missionary"
  (def !n (atom 0))
  (with (p/run (! (let [x (p/watch !n)]
                    (->> (p/fn [] x)                        ; lift or "reactive quote"
                         (m/eduction (map (partial * 10)))  ; flow transducer
                         (m/relieve {})
                         (new)))))                          ; monadic join or "reactive unquote"
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
  (with (p/run (! (let [x (p/watch !n)]
                    (->> (p/fn [] x)
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
