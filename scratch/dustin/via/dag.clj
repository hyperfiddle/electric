(ns dustin.via.dag
  (:require
    [dustin.via.free :refer [Evaluator interpret interpret-1]]
    [clojure.walk :refer [postwalk]]
    [minitest :refer [tests]]
    [missionary.core :as m]))

(defn call [f & args] (apply f args))

(def id-monad
  (reify Evaluator
    (pure [_ c] c)
    (fmap [_ f ma] (f ma))
    (fapply [_ mf ma] (mf ma))
    (fapply [_ mf ma mb] (mf ma mb))
    (bind [_ ma f] (f ma))
    (apply-id [_ f a] (f a))
    (apply-id [_ f a b] (f a b))))

(tests
  (interpret id-monad
    {'boom! (fn [x & args] (println 'boom!) x)}
    (inc ~(boom!. ~(inc 1))))
  ; boom!
  := 3)

(def flow-monad
  (reify Evaluator
    (pure [_ c] (m/relieve {} (m/ap c)))
    (fmap [_ f ma]
      (println 'fmap f ma)
      (m/latest f ma))
    (fapply [_ mf ma] (m/latest call mf ma))
    (fapply [_ mf ma mb] (m/latest call mf ma mb))
    (bind [_ ma f] (m/relieve {} (m/ap (m/?! (f (m/?! ma))))))
    (apply-id [_ f a] (f a))
    (apply-id [_ f a b] (f a b))))

(def effects {'boom! (fn [x & args]
                       (m/relieve {} (m/ap (println 'boom!) x)))})

(tests
  (def !x (atom 0))
  (def x (m/watch !x))
  (interpret flow-monad effects (inc ~x))
  (clojure.walk/postwalk (partial interpret-1 flow-monad effects) '(inc ~x))

  ;@((m/latest inc x) #() #())

  (def y *1)
  (def !y (y #(println :ready) #(println :done)))
  (reset! !x 1)
  )

(tests
  (interpret-1 flow-monad effects '(inc ~x))
  (def z *1)
  (def !z (z #(println :ready) #(println :done)))
  )

;(tests
;  "backpressure 3 w latest"
;  (def !a (atom 0))
;  (def >a (m/watch !a))
;  (def >z (m/latest inc >a))
;  (def !z (>z #(println :ready) #(println :done)))
;  (reset! !a 1)
;  (reset! !a 2)
;  (reset! !a 3)
;  @!z := 4
;  )
