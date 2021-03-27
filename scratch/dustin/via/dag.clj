(ns dustin.via.dag
  (:require
    [dustin.via.free :refer [Interpreter interpret interpret-1 eval-id]]
    [clojure.walk :refer [postwalk]]
    [minitest :refer [tests]]
    [missionary.core :as m]))

(defn call [f & args] (apply f args))

(tests
  (interpret eval-id
    {'boom! (fn [x & args] (println 'boom!) x)}
    (inc ~(boom!. ~(inc 1))))
  ; boom!
  := 3)

(def eval-flow
  (reify Interpreter
    (fapply [_ mf ma] (m/latest call mf ma))
    (fapply [_ mf ma mb] (m/latest call mf ma mb))
    (join [_ mma] (m/relieve {} (m/ap (m/?! (m/?! mma)))))
    (pure [_ c] (m/relieve {} (m/ap c)))))

(tests
  "flow application smoke screen"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def z (interpret-1 eval-flow {} '(inc ~x)))
  (def !z (z #(println :ready) #(println :done)))
  (swap! !x inc)
  (swap! !x inc)
  ;ready
  @!z := 3)

(tests
  "flow interpreter smoke screen"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def z (interpret eval-flow {} (inc ~x)))
  (def !z (z #(println :ready) #(println :done)))
  (swap! !x inc)
  (swap! !x inc)
  ;ready
  @!z := 3)

(tests
  "flow interpreter smoke screen - composition"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def z (interpret eval-flow {} (+ ~(inc ~x) 1)))
  (def !z (z #(println :ready) #(println :done)))
  ;ready
  (swap! !x inc)
  (swap! !x inc)
  @!z := 4)

(def effects {'boom! (fn [x & args]
                       (m/relieve {} (m/ap (println 'boom!) x)))})

(tests
  "flow hello world"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def z (interpret eval-flow effects
           (+ ~(boom!. ~(inc ~x)) 100)))
  (def !z (z #(println :ready) #(println :done)))
  ;ready
  (swap! !x inc)
  (swap! !x inc)
  @!z := 103)
