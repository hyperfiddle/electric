(ns dustin.y2022.missionary-cp
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

(defn odd-flow [x]
  (m/ap
    (m/amb :ignored (m/? (m/sleep 1000 :ignored2)))
    x))

(tests
  (def it ((odd-flow 1) #(prn :ready) #(prn :done)))
  @it)

(tests
  (def it ((m/cp (m/?< (odd-flow :odd))) #(prn :ready) #(prn :done)))
  @it)

(tests
  (def !atom (atom 0))
  (p/run (tap (new (odd-flow (p/watch !atom))))))

(tests
  (let [!atom (atom 0)]
    (with (p/run (tap (new (odd-flow (p/watch !atom)))))
      % := 0
      (swap! !atom inc)
      % := 1
      % := ::rcf/timeout)))
