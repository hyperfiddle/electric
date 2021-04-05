(ns dustin.via.incr2
  "dataflow monad"
  (:require
    [clojure.walk :refer [postwalk]]
    [dustin.via.free3 :as free :refer [fapply join pure inject run boot bind if2
                                       lift-and-resolve]]
    [meander.epsilon :refer [match]]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]
    [missionary.core :as m]))

(defn Eval-incr [effects]
  (reify
    free/Interpreter
    (fapply [M ms] #_(println 'fapply ms)
      (m/signal! (apply m/latest #(apply % %&) ms)))
    (join [M mma] #_(println 'join mma)
      (m/signal! (m/relieve {} (m/ap (m/?! (run M (m/?! mma)))))))
    (pure [M c] (m/ap c))
    (inject [M x] (println 'inject x)
      (match x
        (`deref ?x) (join M ?x)
        ('quote _) x
        (!xs ...) (let [mas (doall (map #(lift-and-resolve M effects %) !xs))
                        mmb (fapply M mas)] (join M mmb))
        _ x))

    (run [M ast] #_(println 'run ast)
      (postwalk #(inject M %) ast))

    (boot [M ast resolve! reject!]
      ((m/reactor (run M ast)) resolve! reject!))

    (boot [M ast]
      (boot M ast #(println ::finished %) #(println ::crashed %)))))

(defn println! [!result x] #_(println 'println! !result x)
  `~(m/ap (swap! !result conj (with-out-str (print x)))))

(tests
  (tests
    (def M (Eval-incr {}))
    (def c 42)
    (pure M c) := _
    (lift-and-resolve M {} `'1)
    (lift-and-resolve M {} `'c)
    (lift-and-resolve M {} `~c) := 42                       ; resolved in clojure
    (lift-and-resolve M {} `c) := 42                        ; resolved by flow
    (lift-and-resolve M {} `'c) := _                        ; (pure M 42)

    (free/resolve' M {'x 11} 'x) := _                       ; pure
    )

  (tests
    "flow apply"
    (def !result (atom []))
    (def !x (atom 0)) (def x (m/watch !x))
    (def eff (partial println! !result))
    (boot M `('eff x))
    (swap! !x inc)
    @!result := ["0" "1"])

  (tests
    "flow eval not sure what actually"
    (def !result (atom []))
    (def !x (atom 0)) (def x (m/watch !x))
    (boot (Eval-incr {`eff (partial println! !result)})
      `(eff x))
    (swap! !x inc)
    @!result := ["0" "1"])

  ; Do effects need to be resolved at runtime?
  ; they can be fixed
  ; it's a lang for user effects tho

  (tests
    "flow composition"
    (def !z (atom []))
    (def !x (atom 0)) (def x (m/watch !x))
    (boot (Eval-incr {`println (partial println! !z)
                      `+       #(m/ap (+ % %2))})
      `(println (+ ~(m/ap (inc (m/?! x))) '100)))
    (swap! !x inc)
    @!z := ["101" "102"])

  (tests
    "flow composition"
    (def !z (atom []))
    (def !x (atom 0)) (def x (m/watch !x))
    (boot (Eval-incr {`println (partial println! !z)
                      `slow!   #(m/ap #_(m/? (m/sleep 1000)) %)}) ; don't sample too soon in tests
      `(println (slow! x)))
    (swap! !x inc)
    @!z := ["0" "1"])

  (tests
    "effect composition"
    (def !z (atom []))
    (def !x (atom 0)) (def x (m/watch !x))

    (defn pr [x] (println! !z x))
    (defn slow! [x] (m/ap (m/? (m/sleep 100)) x))
    (defn bro [x1]
      ; even odd even - should work
      `('slow! ~x1))

    (boot (Eval-incr {})
      `('pr ('bro x)))                                      ; not working ?

    (swap! !x inc)
    @!z := ["0" "1"])

  #_(tests
    "composition"
    (def !z (atom []))
    (def !x (atom 0)) (def x (m/watch !x))
    (def !c (atom :odd)) (def c (m/watch !c))
    (def !d (atom :even)) (def d (m/watch !d))

    (boot (Eval-incr {})
      `(println ('foo x)))

    (swap! !x inc)
    (swap! !x inc)
    @!z := [":even" ":odd" ":even"])

  )

(defn log! [!result x] (m/ap (swap! !result conj x)))

(tests
  "if"
  (def !result (atom []))
  (def !x (atom 0)) (def x (m/watch !x))
  (def !c (atom :odd)) (def c (m/watch !c))
  (def !d (atom :even)) (def d (m/watch !d))

  (boot (Eval-incr {`log (partial log! !result)
                    `if  if2})
    `(log
       (if ~(m/ap (odd? (m/?! x)))
         'c 'd)))

  (swap! !x inc)
  (swap! !x inc)
  @!result := [:even :odd :even])

(tests
  "if take 2"
  (do
    (def !result (atom []))
    (def !x (atom 0)) (def x (m/watch !x))
    (def !c (atom :odd)) (def c (m/watch !c))
    (def !d (atom :even)) (def d (m/watch !d)))

  (defn odd?2 [x] (m/ap (odd? x)))

  (boot (Eval-incr {`log (partial log! !result)
                    `if  if2})
    `(log (if ('odd?2 x)
            'c 'd)))

  (swap! !x inc)
  (swap! !x inc)
  @!result := [:even :odd :even])

(defn lift [f]
  (m/ap
    (fn [& args]
      (m/ap (apply f args)))))

(tests
  "if take 3"
  (do
    (def !result (atom []))
    (def !x (atom 0)) (def x (m/watch !x))
    (def !c (atom :odd)) (def c (m/watch !c))
    (def !d (atom :even)) (def d (m/watch !d)))

  (boot (Eval-incr {`log (partial log! !result)
                    `if  if2})
    `(log (if (~(lift odd?) x) 'c 'd)))

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
