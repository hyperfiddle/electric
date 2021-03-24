(ns dustin.reactor1
  (:require
    [minitest :refer [tests]]
    [missionary.core :as m :refer [? ?? ?! ap]]))

(defn sleep-emit [delays]
  (ap (let [n (?? (m/enumerate delays))]
        (? (m/sleep n n)))))

(do
  (def !x (atom 0))
  (def effect (partial println 'effect))

  (def process (m/reactor
                 (let [>a (m/signal! (m/watch !x))
                       >b (m/stream! (m/sample #(first %&)
                                       >a
                                       (sleep-emit (take 10 (repeat 100)))))]
                   (m/stream!
                     (ap (effect (?! >b)))))))
  (process
    (partial println 'finished 'success)
    (partial println 'finished 'failure))

  (swap! !x inc)
  )

(comment
  (def !x (atom 0))

  (def effect (partial println 'effect))

  (def process
    (m/reactor
      ; reactors produce effects
      (try
        (let [>a (m/signal! (m/watch !x))

              #_#_
              >z (m/signal! (m/latest vector
                              (m/relieve {} (ap (inc (?! >a))))
                              (m/relieve {} (ap (dec (?! >a))))))]
          (ap (effect (m/sample first >a (sleep-emit [100 100 100 100 100]))))
          #_(ap (effect (?! >z)))
          #_(declare !z)
          #_(def !z (>z #(prn :ready) #(prn :done))))
        (catch Exception e (println 'error)))))



  (def ctx (process
             (partial println 'finished 'success)
             (partial println 'finished 'failure)))


  ((m/sp (let [>z (m/? process)
               !z (>z #(prn :ready) #(prn :done))]

           ))
   #(prn :ready1) #(prn :ready2))

  (swap! !x inc)
  (effect :z)

  ;(def !out (>out #(prn :ready) #(prn :done)))
  ;@!out

  ;(def process-cancel (process #(prn :process/success) #()))

  )

(comment

  ; cancelled
  (def r (m/reactor (m/signal! (m/ap))))
  (m/? r)

  (m/reactor
    (let [r (atom [])
          i (m/signal! (m/watch (atom 1)))]
      (m/stream! (m/ap (m/?? i) (swap! r conj (m/?? i)))) r))

  )


