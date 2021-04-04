(ns dustin.via.incr1
  "dataflow"
  (:require
    [dustin.via.free :refer [Interpreter interpret interpret-1 eval-id
                             fapply join pure bind if2]]
    [minitest :refer [tests]]
    [missionary.core :as m]))

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
  (def z (interpret-1 eval-flow {} '(inc @x)))
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
           (inc @x)))
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
           (+ @(inc @x) 1)))
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
           (+ @(boom!. @(inc @x)) 100)))
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

(defn println! [!result x]
  (m/ap (swap! !result conj (with-out-str (print x)))))

(tests
  "managed incremental eval"
  (def !result (atom []))
  (def !x (atom 0))
  (def x (m/watch !x))

  (run-incr {'println (partial println! !result)}
    (println. @(+ @(inc @x) 100)))

  (swap! !x inc)
  (swap! !x inc)
  @!result := ["101" "102" "103"])

(tests
  (def !result (atom []))
  (def !x (atom 0)) (def x (m/watch !x))
  (def !c (atom :odd)) (def c (m/watch !c))
  (def !d (atom :even)) (def d (m/watch !d))

  (defn foo [x]
    (if (odd? x)
      (interpret eval-incr effects (identity @c))
      (interpret eval-incr effects (identity @d))))

  (run-incr {'println (partial println! !result)
             'foo foo}
    (println. @(foo. @x)))

  (swap! !x inc)
  (swap! !x inc)
  @!result := [":even" ":odd" ":even"])

(defn log! [!result x] (m/ap (swap! !result conj x)))

(tests
  "if"
  (def !result (atom []))
  (def !x (atom 0)) (def x (m/watch !x))
  (def !c (atom :odd)) (def c (m/watch !c))
  (def !d (atom :even)) (def d (m/watch !d))

  (run-incr {'log (partial log! !result)
             'if  if2}
    (log.
      @(if. @(odd? @x)
         (identity @c)
         (identity @d))))

  (swap! !x inc)
  (swap! !x inc)
  @!result := [:even :odd :even])

; foreach2 :: Flow List a -> (a -> Flow b) -> Flow Flow List b
;(defn foreach2 [>xs eff] (m/ap (foreach (m/?! >xs) eff)))

;(tests
;  "foreach"
;  (def !xs (atom [{:id 1 :name "alice"} {:id 2 :name "bob"}]))
;  (def xs (m/watch !xs))
;
;  (def !result (atom []))
;  (run-incr {'println (partial println! !result)
;             'foreach foreach}
;    (foreach. @xs println.)
;    (foreach. @xs (fn [x] (println. x)))
;    (for [x @xs] (inc x))                                   ; for macro lifts result
;    (for [>x @xs] (inc @>x))
;    )
;
;  (swap! !x inc)
;  (swap! !x inc)
;  @!result := [:even :odd :even])
