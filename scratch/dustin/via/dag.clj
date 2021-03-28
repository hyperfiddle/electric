(ns dustin.via.dag
  (:require
    [dustin.via.free :refer [Interpreter interpret interpret-1 eval-id]]
    [clojure.walk :refer [postwalk]]
    [minitest :refer [tests]]
    [missionary.core :as m]))

(tests
  "identity monad, managed effect"
  (interpret eval-id
    {'boom! (fn [x & args] (println 'boom!) x)}
    (inc ~(boom!. ~(inc 1))))
  ; boom!
  := 3)

(defn call [f & args] (apply f args))

(def eval-flow
  (reify Interpreter
    (fapply [_ ms] (apply m/latest call ms))
    (join [_ mma] (m/relieve {} (m/ap (m/?! (m/?! mma)))))
    (pure [_ c] (m/ap c))))

(tests
  "flow apply"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def z (interpret-1 eval-flow {} '(inc ~x)))
  (def !z (z #(println :ready) #(println :done)))
  (swap! !x inc)
  (swap! !x inc)
  ;ready
  @!z := 3)

(tests
  "flow eval"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def z (interpret eval-flow {}
           (inc ~x)))
  (def !z (z #(println :ready) #(println :done)))
  (swap! !x inc)
  (swap! !x inc)
  ;ready
  @!z := 3)

(tests
  "flow composition"
  (def !x (atom 0))
  (def x (m/watch !x))
  (def z (interpret eval-flow {}
           (+ ~(inc ~x) 1)))
  (def !z (z #(println :ready) #(println :done)))
  ;ready
  (swap! !x inc)
  (swap! !x inc)
  @!z := 4)

(def effects {'boom! (fn [x & args]
                       (m/ap
                         #_(m/? (m/sleep 1000))
                         (println 'boom!)
                         x))})

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

(def eval-incr
  (reify Interpreter
    (fapply [_ ms] (m/signal! (apply m/latest call ms)))
    (join [_ mma] (m/signal! (m/relieve {} (m/ap (m/?! (m/?! mma))))))
    (pure [_ c] (m/ap c))))

(defmacro run-incr [effects ast]
  `((m/reactor
      (interpret eval-incr ~effects ~ast))
    (fn [_#] (println :process :finished))
    (fn [e#] (println :process :crashed e#))))

(tests
  "managed incremental eval"

  (def !x (atom 0))
  (def x (m/watch !x))

  (def !result (atom []))

  (defn println! [x & args]
    (m/ap (swap! !result conj (with-out-str (print x)))))

  (run-incr {'println println!}
    (println. ~(+ ~(inc ~x) 100)))

  (swap! !x inc)
  (swap! !x inc)
  @!result := ["101" "102" "103"])

(tests
  (def !x (atom 0)) (def x (m/watch !x))
  (def !c (atom :odd)) (def c (m/watch !c))
  (def !d (atom :even)) (def d (m/watch !d))

  (def !result (atom []))
  (defn println! [x & args]
    (m/ap (swap! !result conj x)))

  (defn foo [x]
    (if (odd? x)
      (interpret eval-incr effects (identity ~c))
      (interpret eval-incr effects (identity ~d))))

  (run-incr {'println println!
             'foo foo}
    (println. ~(foo. ~x)))

  (swap! !x inc)
  (swap! !x inc)
  @!result := [:even :odd :even])

(tests
  (def !x (atom 0)) (def x (m/watch !x))
  (def !c (atom :odd)) (def c (m/watch !c))
  (def !d (atom :even)) (def d (m/watch !d))

  (def !result (atom []))
  (defn println! [x & args]
    (m/ap (swap! !result conj x)))

  (defn if2 [test >x >y]
    (m/ap (m/?! (if test >x >y))))

  (run-incr {'println println!
             'if if2}
    (println.
      ~(if. ~(odd? ~x)
         (identity ~c)
         (identity ~d))))

  (swap! !x inc)
  (swap! !x inc)
  @!result := [:even :odd :even])

