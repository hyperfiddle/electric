(ns user.photon-1-hello-world
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]))


(hyperfiddle.rcf/enable!)

(tests "hello world"
  (def dispose (p/run (rcf/! ::x)))
  % := ::x
  (dispose))

(tests "react based on a ref"
  (def !x (atom 0))
  (def dispose (p/run (! (p/watch !x))))
  % := 0
  (swap! !x inc)
  % := 1
  (dispose))

(tests "dataflow diamond"
  (def !x (atom 0))
  (def dispose
    (p/run
      (let [x (p/watch !x)]                                 ; one watch, shared
        (! (+ x x)))))
  % := 0
  (swap! !x inc)
  % := 2
  (swap! !x inc)
  % := 4
  (dispose))

(tests "broken dataflow diamond"
  (def !x (atom 0))
  (def dispose
    (p/run (! (+ (p/watch !x) (p/watch !x)))))              ; bad - two watches on the same ref - do not do this
  % := 0
  (swap! !x inc)                                            ; each watch fires an event resulting in two propagation frames
  % := 1                                                    ; bad
  % := 2
  (swap! !x inc)
  % := 3                                                    ; bad
  % := 4
  (dispose))

(tests
  "auto-dispose test macro"
  (with (p/run (rcf/! ::x))
    % := ::x))

(tests "reactive function call"
  (def !x (atom 1))
  (def !f (atom +))
  (with (p/run (let [f (p/watch !f)
                     x (p/watch !x)]
                 (! (f 0 x))))
    % := 1
    (swap! !x inc)
    % := 2
    (reset! !f -)
    % := -2))

(tests
  "foreign clojure functions including core. map is not incremental, the args are"
  (def !xs (atom [1 2 3]))
  (def !f (atom inc))
  (with (p/run (! (let [f  (p/watch !f)
                        xs (p/watch !xs)]
                    (clojure.core/map f xs))))
    % := [2 3 4]
    (swap! !xs conj 4)
    % := [2 3 4 5]
    (reset! !f dec)
    % := [0 1 2 3]))

(tests
  "reactive defn; these are reactive fns and called with . (analogous to hiccup).
  best example of this is virtual dom incremental maintenance"

  (p/def trace!)                                            ; all photon defs are dynamic
  (p/defn Div [x] (trace! x) [:div x])                      ; reactive fns are TitleCase like React components
  (p/defn Widget [x] (Div. [(Div. x) (Div. :a)]))           ; . marks reactive call

  (def !x (atom 0))
  (with (p/run (! (binding [trace! !]
                    (Widget.                                ; . marks reactive call
                      (p/watch !x)))))
    % := 0
    % := :a
    % := [[:div 0] [:div :a]]
    % := [:div [[:div 0] [:div :a]]]
    (swap! !x inc)
    % := 1
    ; no :a
    % := [[:div 1] [:div :a]]
    % := [:div [[:div 1] [:div :a]]]))

(tests
  "reactive closures"
  (def !x (atom 1))
  (def !y (atom 10))
  (p/def x (p/watch !x))
  (p/def y (p/watch !y))
  (with (p/run (! (new (if (odd? x)
                         (p/fn [x] (* y x))
                         (p/fn [x] (* y x)))
                       x)))
    % := 10
    (swap! !x inc)
    % := 20
    (swap! !x inc)
    % := 30
    (swap! !y inc)
    % := 33
    (swap! !y inc)
    % := 36))

(tests
  "reactive for with stable body (mount/unmount lifecycle)"
  (def !xs (atom [1 2 3]))
  (with (p/run (! (p/for [x (p/watch !xs)]
                    (inc (! x)))))
    (hash-set % % %) := #{2 1 3}                            ; the for body is visited concurrently
    % := [2 3 4]                                            ; the end result is ordered
    (swap! !xs conj 4)                                      ; incremental update
    % := 4                                                  ; only the new item is visited
    % := [2 3 4 5]))
