(ns user.rcf-async
  (:require [hfdl.lang :as r :refer [defnode node]]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

(def !x (atom 0))
(def x (m/watch !x))
(defnode []
  (tests
    (+ x x) := 0
    ! (swap! !x inc) := 2
    ! (swap! !x inc) := 4

    ))

; Is async handled by tests macro or the language?

(set! rcf/timeout 100)
; helper that accepts an atom and a check against that atom

; test async expressions (in flow)
; test for side effects

(tests

  (def !x (atom 0))
  (def x (m/watch !x))

  ~(r/run (! (+ x x))) := 0
  (swap! !x inc) ! := 2
  (swap! !x inc) ! := 4


  :> (r/run (! (+ x x))) := 0
  (swap! !x inc) := 2
  (swap! !x inc) := 4)

(tests
  (inc 1) := 2

  (def !x (atom 0))
  (def x (m/watch !x))

  ~(r/run (+ x x)) := 0
  ! (swap! !x inc) := 2
  ! (swap! !x inc) := 4

  (rcf/async
    (photon/test (+ x x)) := 0
    ! (swap! !x inc) := 2
    ! (swap! !x inc) := 4)

  ; how do you check something with a timeout
  ;! (do (swap! !x inc) (Thread/sleep 100)) := 4

  ; tear down test when no more assertions are expected
  ! (rcf/done)

  ; magic callback
  (async done (r/run (+ x x)) := 0 (done))

  ; new idea
  (def !x (atom 0))
  (def x (m/watch !x))
  (doto (rcf/async !
                   (r/run (! (+ x x)))
                   (swap! !x inc)
                   (swap! !x inc)
                   )
    (= 0)
    (= 2)
    (= 4))



  ; idea
  (def !x (atom 0))
  (def x (m/watch !x))
  (def program (r/run (+ x x)))
  program :async= 0
  (swap! !x inc)
  program :async= 2
  (swap! !x inc)
  program :async= 4

  ; questions and requirements

  ; multi threading - with via - you could swap an atom, swap returns immediately, and
  ; there was an effect in response to the swap, but the effect was in a different thread
  ; due to concurrency, which means you don't see it right away, you want to see the result of
  ; the effect given a certain timeout. you want to say "Fail if this is not true in 10ms"
  ;
  ;; lets say you have a reaction in a watch and sequenced with this is several effects
  ; you want to check the results of the effects and oyu know they're concurrent
  ; i want both of these effects to check, but in whatever order


  (def !x (atom 0))
  ~(future (reset! !x 1))
  @!x := 1

  ; future test 2
  (def !x (atom 0))
  ~(future (reset! !x 1) (Thread/sleep 100) @!x := 1
           (reset !x 2) (Thread/sleep 100) @!x := 2
           )



  )

















(tests

  ~(r/run 1) := 1                                           ; literals are lifted
  ~(r/run inc) := inc
  ~(r/run {:a 1}) := {:a 1}                                 ; data literals lifted

  ~(r/test (type #'1)) := 'flow                             ; reactive quote escapes to flow layer
  ~(r/test ~#'1) := 1                                       ; monadic join on nested flow

  (def a 1)
  ~(r/run a) := 1                                           ; globals lifted
  ~(r/run (inc 1)) := 2                                     ; clojure call
  ~(r/run (inc (inc 1))) := 3

  (def !x (atom 0))                                         ; atoms model variable inputs
  (def x (m/watch !x))                                      ; clojure flow derived from atom
  ~(r/run ~x) := 0                                          ; monadic join (weave) foreign flow
  ! (swap! !x inc) := 1                                     ; reactive result



  (def !xs (atom [1 2 3]))
  (def !f (atom inc))
  ~(r/run
     (let [f ~(m/watch !f)
           xs ~(m/watch !xs)]
       (map f xs))) := [2 3 4]                              ; clojure core interop

  ! (swap! !xs conj 4) := [2 3 4 5]
  ! (reset! !f dec) := [0 1 2 3]

  ~(r/run
     (let [f ~(m/watch (atom inc))
           xs ~(m/watch (atom [1 2 3]))]
       (->> xs (map f)))) := [2 3 4]                        ; clojure core macros

  ~(r/run (let [[a] ~(m/watch (atom [:a]))] a)) := :a       ; destructuring

  (def !f (atom +)) (def f (m/watch !f))
  (def !x (atom 1)) (def x (m/watch !x))
  ~(r/run (f 0 x)) := 1                                     ; reactive function call
  ! (swap! !x inc) := 2
  ! (reset! !f -) := -2

  ; transition between running and terminated
  (def !x (atom 0))
  ~(r/run ~(->> (m/watch !x) (m/eduction (take-while even?))))
  := 0
  ! (reset! !x 2) := 2
  ! (reset! !x 1) := 2                                      ; terminated before turning odd

  (def !a (atom 1)) (def a (m/watch !a))
  (def !p (atom :p)) (def p ~(m/watch !p))
  (def !q (atom :q)) (def q ~(m/watch !q))
  ~(r/run (if (odd? a) p q)) := :p                          ; reactive if
  ! (swap! !a inc) := :q
  ! (reset! !p :pp)
  ! (swap! !a inc) := :pp

  (def !a (atom 0)) (def a (m/watch !a))
  (def !p (atom :p)) (def p ~(m/watch !p))
  (def !q (atom :q)) (def q ~(m/watch !q))
  ~(r/test (case a 0 p q)) := :p
  ! (swap! !a inc) := :q
  ! (reset! !q :qq) := :qq

  ; reactive for

  )