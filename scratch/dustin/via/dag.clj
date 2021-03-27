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
    (fapply [_ mf ma] (m/signal! (m/latest call mf ma)))
    (fapply [_ mf ma mb] (m/signal! (m/latest call mf ma mb)))
    (join [_ mma] (m/signal! (m/relieve {} (m/ap (m/?! (m/?! mma))))))
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

(defmacro run-incr [effects ast]
  `((m/reactor
      (m/signal!
        (interpret eval-flow ~effects ~ast)))
    (fn [_#] (println :process :finished))
    (fn [e#] (println :process :crashed e#))))

(tests
  "reactor"

  (def !x (atom 0))
  (def x (m/watch !x))

  (def !result (atom []))

  (defn println! [x & args]
    (m/relieve {}
      (m/ap
        (swap! !result conj
          (with-out-str
            (print '! x))))))

  (run-incr {'println println!}
    (println. ~(+ ~(inc ~x) 100)))

  (swap! !x inc)
  (swap! !x inc)
  @!result := ["! 101" "! 102" "! 103"]

  )

#_
(tests
  "fib in parallel"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def z (interpret eval-flow effects
           (+ ~(boom!. ~(inc ~x)) 100)))
  (def !z (z #(println :ready) #(println :done)))
  ;ready
  (swap! !x inc)
  (swap! !x inc)
  @!z := 103)
