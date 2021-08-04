(ns dustin.missionary5_reactor
  (:require
    [minitest :refer [tests]]
    [missionary.core :as m :refer [? ?? ?! ap]]))

(defn sleep-emit [delays]
  (ap (let [n (?? (m/enumerate delays))]
        (? (m/sleep n n)))))

(tests
  (? (m/aggregate conj (sleep-emit [100 200 300])))
  ; 600ms delay
  := [100 200 300]

  (? (m/aggregate conj (sleep-emit (take 10 (range 100 1000)))))
  ; 1s delay
  := [100 101 102 103 104 105 106 107 108 109])

(tests
  "reactor hello world"

  (def !effects (atom []))
  (defn effect! [%] (swap! !effects conj `(println ~%)))

  (def !x (atom 0))

  (def process (m/reactor
                 (let [>a (m/signal! (m/watch !x))
                       >b (m/stream! (m/sample #(first %&)
                                       >a
                                       (sleep-emit (take 10 (repeat 10)))))]
                   (m/stream!
                     (ap (effect! (?? >b)))))))

  #_(? process)                                             ; hang, reactor never terminates
  #_(? (m/timeout 1000 process))                            ; works

  (process                                                  ; async version
    (partial println 'finished 'success)
    (partial println 'finished 'failure))

  (swap! !x inc)
  (swap! !x inc)
  (Thread/sleep 1000)
  (set @!effects) := '#{(clojure.core/println 0)
                        (clojure.core/println 1)
                        (clojure.core/println 2)}
  )

