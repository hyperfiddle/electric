(ns user.lang1
  (:require [hfdl.lang :as photon :refer [defnode node]]
            [hyperfiddle.q2 :refer [q nav hfql]]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

; node vs fn
(def !x (atom 0))
(defn f [x] x)
(defnode g [x] x)
(defn call [f x] (f x))                                     ; value

(tests
  ~(photon/test
     (let [ff (fn [x] x)                                    ; useful for passing event callbacks to foreign code
           gg (node [x] x)                                  ; you almost always want this
           x ~(m/watch !x)]
       [(f x)                                               ; var says foreign
        (g x)                                               ; var says node
        (gg x)                                              ; assume node
        (call ff x)                                         ; needs a hint
        ]))
  := [0 0 0 0])

; reactive closures

(defnode reactive-call [g x] (g x))                         ; assume node
(def !x (atom 0))
(def !y (atom 0))

(tests
  ~(photon/test
     (let [x ~(m/watch !x)
           y ~(m/watch !y)
           f (node [needle] (+ y needle))                   ; constant signal
           g ~(m/seed [(node [needle] (+ y needle))])]
       [(reactive-call f x)
        (reactive-call ~g x)]))                             ; ?
  := [0 0])

; reactive closure over discarded var
; if we really want to be able to close over reactive values we
; need to solve the problem of dynamic extent. if a node closes over a
; reactive value and this value is destroyed due to a conditional switching,
; what happens ?

(def !x (atom 0))
(def !y (atom 0))

(tests
  ~(photon/test
     ($ (let [x ~(m/watch !x)
              y ~(m/watch !y)
              z (if (even? x) x y)]
          (node [] z))))
  := 0
  (swap! !x inc)
  := 0)

(tests
  ~(photon/test
     ($ (let [x ~(m/watch !x)
              y ~(m/watch !y)]
          (if (even? x)
            (node [] x)
            (node [] y)))))
  := 0
  (swap! !x inc)
  := 0)

(tests
  ~(photon/test
     (let [x ~(m/watch !x)
           y (if (odd? x) ~(m/watch !x) ~(m/watch !y))
           f (node [needle] (+ y needle))]
       (reactive-call f x)))
  := 0
  (swap! !x inc)
  := 0)

