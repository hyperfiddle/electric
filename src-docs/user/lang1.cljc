(ns user.lang1
  "Photon language tutorial"
  (:require [hfdl.lang :as r :refer [defnode node]]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

(tests
  "tutorial"
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

  (def !x (atom 0))
  (def x (m/watch !x))
  ~(r/run (+ x x)) := 0
  ! (swap! !x inc) := 2
  ! (swap! !x inc) := 4

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

(test
  "effects"

  ; dags are RT other than clojure interop and their execution is optimized

  )

; let introduces shared node
; node call (static dispatch)

; nested dags


; first-class nodes seem inevitable
; first-class nodes seems easily solvable, but we'll need syntax disambiguate
; ordinary function calls from node calls

(defnode g [x] x)                                           ; reactive fn (DAG). Compiler marks dag with meta
(defn f [x] x)                                              ; This var is not marked with meta

(tests
  "higher order dags"
  (def !x (atom 0))
  ~(r/run
     (let [ff (fn [x] x)                                    ; foreign clojure fns are useful, e.g. passing callbacks to DOM
           gg (node [x] x)                                  ; you almost always want this, not fn
           x ~(m/watch !x)]
       [(f x)                                               ; var marked
        (g x)                                               ; var says node
        (ff x)                                              ; Must assume interop, for compat with clojure macros
        (r/call gg x)                                       ; Must mark reactive-call
        (r/call (node [x] x) x)]))
  := [0 0 0 0 0])

(tests
  "reactive node closure"
  (def !x (atom 0))
  (def !y (atom 0))
  ~(r/run
     (let [x ~(m/watch !x)
           y ~(m/watch !y)
           _ 1
           _ ~(m/seed [1])
           f (node [needle] (+ y needle))                   ; constant signal
           g (if (odd? x) (node [needle] (+ y needle)) (node [needle] (+ y needle)))
           h ~(m/seed [(node [needle] (+ y needle))])]
       [(call f x)
        (call g x)
        (call h x)]))
  := [0 0 0])

(tests
  "reactive clojure.core/fn"
  ~(r/run
     (let [x ~(m/watch !x)
           y ~(m/watch !y)
           f (fn [needle] (+ y needle))]                    ; closure is rebuilt when flow changes
       ; (value is fully compatible with fn contract)
       ; the lambda is as variable as the var it closes over
       ; well defined. It's not allowed to use dataflow inside FN. Compiler can never reach it
       ; compiler will talk it to detect the free variables only
       (f x)))
  := [0 0])

; if we really want to be able to close over reactive values we
; need to solve the problem of dynamic extent. if a node closes over a
; reactive value and this value is destroyed due to a conditional switching,
; what happens ?
; In other words, there is a dag alive that needs X and X dies
; Should that dag be killed as well, or allowed to live with last known value of x, or undefined?

(tests
  "reactive closure over discarded var"
  (def !a (atom false))
  (def !b (atom 1))
  ~(r/run
     (call (let [!n (atom (node [] 0))]
             (when ~(m/watch !a)
               (let [x ~(m/watch !b)]
                 (reset! !n (node [] x))))
             ~(m/watch !n))))
  := 0
  (swap! !a not)
  := 1
  (swap! !a not)                                            ; watch !b is discarded
  := 'undefined)

(defnode fib [n]
  (case n
    0 0 1 1
    (+ (recur (- n 2))
       (recur (- n 1)))))

(tests
  "reactive recursion"
  (fib 3) := 5)

(declare pong)
(defnode ping [x] (case x 0 :done (pong (dec x))))
(defnode pong [x] (ping x))

(tests
  "mutual recursion"
  (ping 3) := :done)

(defnode boom [] (throw (Exception. "")))

(tests
  "exceptions"
  ; As a reference, see how regular Clojure exceptions are dynamic scoped
  (try
    (let [f (try (fn [] (/ 1 0))                            ; this exception will escape
                 (catch Exception _ :inner))]
      ; the lambda doesn't know it was constructed in a try/catch block
      (f))
    (catch Exception _ :outer))
  := :outer

  ~(r/run (r/try (boom) (r/catch Exception _))) := nil      ; reactive exception caught

  ~(r/run
     (r/try
       (let [f (r/try
                 (node [] (boom))                           ; reactive exception uncaught
                 (r/catch Exception _ :inner))]
         (f))
       (r/catch Exception _ :outer)))
  := :outer)

(defnode db)
(defnode foo [] db)

(tests
  "dynamic scope (note that try/catch has the same structure)"
  ~(r/run (r/with {db 0} (foo))) := 0                       ; binding available

  ~(r/run (r/with {db 1}
                  (let [n (r/with {db 0}
                                  (node [] (foo)))]         ; binding out of scope
                    (r/call n)))) := 1)

; first class node with static linking
; first class node with dynamic linking