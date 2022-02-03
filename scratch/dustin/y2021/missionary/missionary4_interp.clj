(ns dustin.missionary4_interp
  (:require
    [hyperfiddle.via :refer [via Do-via !]]
    [minitest :refer [tests]]
    [missionary.core :as m]))


(deftype Fabric []
  Do-via
  (resolver-for [R]
    {:Do.fmap   (fn [f >a] (m/latest f >a))
     :Do.pure   (fn [a] (m/watch (atom a)))
     :Do.fapply (fn [>f & >as]
                  (apply m/latest #(apply % %&) >f >as))
     :Do.bind   (fn [>a f] (m/relieve {} (m/ap (m/?! (f (m/?! >a))))))
     }))

(tests
  "applicative interpreter"

  (def !a (atom nil))
  (def >a (m/watch !a))
  (def >z (via (->Fabric)
               (let [>b (inc ~>a)
                     >c (dec ~>a)]
                 (vector ~>b ~>c :x))))

  (def !z (>z #(println :ready) #(println :done)))

  (reset! !a 1)
  @!z := [2 0 :x]
  (reset! !a 2)
  (reset! !a 3)
  @!z := [4 2 :x]

  )

(tests
  "monad interpreter"

  )
