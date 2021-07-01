(ns user.lang1
  (:require [hfdl.lang :as * :refer [defnode node $]]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

; first-class nodes seem inevitable
; first-class nodes seems easily solvable, but we'll need syntax disambiguate
; ordinary function calls from node calls

(defn f [x] x)
(defnode g [x] x)

(tests
  "node vs fn"
  (def !x (atom 0))
  ~(photon/test
     (let [ff (fn [x] x)                                    ; useful for passing event callbacks to foreign code
           gg (node [x] x)                                  ; you almost always want this
           x ~(m/watch !x)]
       [(f x)                                               ; var says foreign
        (g x)                                               ; var says node
        (ff x)                                              ; Must assume interop, for compat with clojure macros
        ($ gg x)                                            ; Must mark reactive-call
        ($ (node [x] x) x)]))
  := [0 0 0 0 0])

(tests
  "reactive node closure"
  (def !x (atom 0))
  (def !y (atom 0))
  ~(photon/test
     (let [x ~(m/watch !x)
           y ~(m/watch !y)
           _ 1
           _ ~(m/seed [1])
           f (node [needle] (+ y needle))                   ; constant signal
           g (if (odd? x) (node [needle] (+ y needle)) (node [needle] (+ y needle)))
           h ~(m/seed [(node [needle] (+ y needle))])]
       [($ f x)
        ($ g x)
        ($ h x)]))
  := [0 0 0])

(tests
  "reactive clojure.core/fn"
  ~(photon/test
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
  ~(photon/test
        ($ (let [!n (atom (node [] 0))]
             (when ~(m/watch !a)
               (let [x ~(m/watch !b)]
                 (reset! !n (node [] x))))
             ~(m/watch !n))))
  := 0
  (swap! !a not)
  := 1
  (swap! !a not)                                            ; watch !b is discarded
  := 'undefined)

(tests
  "exception that is uncaught due to handler going out of scope
  (This is a parallel example to explain photon/with)"
  (try
    (let [f (try (fn [] (/ 1 0))
                 (catch Exception _ :inner))]
      ; the lambda doesn't know it was constructed in a try/catch block
      (f))
    (catch Exception _ :outer))
  := :outer)

(defnode db)
(defnode foo [] db)

(tests
  "binding that is used"
  ~(photon/test
     (photon/with {db 0} (foo)))
  := 0)

(tests
  "binding that goes out of scope"
  ~(photon/test
     (photon/with {db 1}
       (let [n (photon/with {db 0} (node [] (foo)))]
         (photon/$ n))))
  := 1)

(defnode boom [] (throw (Exception. "")))

(tests
  "dataflow exception"
  ~(photon/test
     (photon/try
       (boom)
       (photon/catch Exception _)))
  := nil)

(tests
  "dataflow exception that goes out of scope"
  ~(photon/test
     (photon/try
       (let [f (photon/try
                 (node [] (boom))
                 (photon/catch Exception _ :inner))]
         (f))
       (photon/catch Exception _ :outer)))
  := :outer)

; first class node with static linking
; first class node with dynamic linking
