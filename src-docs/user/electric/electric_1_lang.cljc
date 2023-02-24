(ns user.electric.electric-1-lang
  "Introduction to Hyperfiddle Photon, a fully async and reactive Clojure/Script dialect,
  designed to express a distributed client/server system in a single program, with
  compiler-managed client/server data sync.

  More info:
    https://www.hyperfiddle.net/
    https://twitter.com/dustingetz/status/1520397540386091009"

  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]))


(hyperfiddle.rcf/enable!)

(tests "hello world"
  (def dispose (e/run (rcf/tap ::x)))
  % := ::x
  (dispose))

(tests "react based on a ref"
  (def !x (atom 0))
  (def dispose (e/run (rcf/tap (e/watch !x))))
  % := 0
  (swap! !x inc)
  % := 1
  (dispose))

(tests
  "The Electric Clojure process is stateful and runs until disposed. All reactive subscriptions are kept
  alive, essentially memoizing past results so that future renders can reuse cached intermediate
  results that don't need to be recomputed. This cache state is managed by the underlying missionary
  reactor which saves past \"reactive stack frames\" until the process is disposed. In this way,
  reactive programming is trading space (cached intermediate results) for time (skipping work that
  doesn't need to be recomputed)."
  (def !a (atom 1))
  (def !b (atom 1))
  (def dispose
    (e/run (let [a (e/watch !a) ; a is cached
                 b (e/watch !b)] ; b is cached
             (tap (+ a b)))))
  % := 2
  (swap! !a inc) ; cached b is reused
  % := 3
  (swap! !b inc) ; cached a is reused
  % := 4
  (dispose)) ; caches consume memory until disposed

(tests "dataflow diamond"
  (def !x (atom 0))
  (def dispose
    (e/run
      (let [x (e/watch !x)
            a (inc x)
            b (inc x)]
        (tap (+ a b)))))
  % := 2
  (swap! !x inc)
  % := 4
  (swap! !x inc)
  % := 6
  (dispose))

(tests
  "auto-dispose test macro"
  (with (e/run (rcf/tap ::x))
    % := ::x))

(tests "reactive function call"
  (def !x (atom 1))
  (def !f (atom +))
  (with (e/run (let [f (e/watch !f)
                     x (e/watch !x)]
                 (tap (f 0 x))))
    % := 1
    (swap! !x inc)
    % := 2
    (reset! !f -)
    % := -2))

(tests
  "interop to foreign clojure functions including clojure.core"
  (def !xs (atom [1 2 3]))
  (def !f (atom inc))
  (with (e/run (tap (let [f  (e/watch !f)
                          xs (e/watch !xs)]
                    (clojure.core/map f xs))))              ; foreign call
    % := [2 3 4]
    (swap! !xs conj 4)
    % := [2 3 4 5]
    (reset! !f dec)
    % := [0 1 2 3]))

(tests
  "p/def can contain Photon expressions"
  (def !x (atom 0))                                         ; def form is Clojure
  (e/def x (e/watch !x))                                    ; e/def form is Photon
  (with (e/run (tap x))
    % := 0
    (swap! !x inc)
    % := 1))

(tests "reactive defn"
  (e/defn Foo [x trace!]
    (+ (trace! (inc 10))
       (trace! (inc x))))

  (def !x (atom 0))
  (with (e/run (Foo. (e/watch !x) tap))
    % := 11
    % := 1
    (swap! !x inc)
    ;% := 11 - constant signal didn't change
    % := 2))

(tests "Photon defs are dynamic by default"
  (def !x (atom 0))
  (e/def x)                                                 ; ^:dynamic implied, no need to mark
  (e/defn Bar [] x)                                         ; x is implicit (from dynamic scope)
  (with (e/run
          (binding [x (e/watch !x)]                         ; bind dynamic scope
            (tap (Bar.))))
    % := 0
    (swap! !x inc)
    % := 1))

(tests
  "reactive defn – these are reactive fns and called with a . like `(Div.)`
  best example of this is virtual dom incremental maintenance"

  (e/def trace!)

  ; reactive fns are TitleCase like React components
  (e/defn Div [x]
    [:div (trace! x)])

  (e/defn Widget "pretend virtual dom" [x]
    (Div.
      [(Div. x)
       (Div. :a)]))

  (def !x (atom 0))
  (with (e/run (binding [trace! tap]
                 (Widget.                                   ; . marks reactive call
                   (e/watch !x))))
    % := 0
    % := :a
    % := [[:div 0] [:div :a]]
    (swap! !x inc)
    % := 1
    ; no :a
    % := [[:div 1] [:div :a]]))

(tests
  "reactive closures"
  (def !x (atom 1))
  (def !y (atom 10))
  (e/def x (e/watch !x))
  (e/def y (e/watch !y))
  (with (e/run (tap (let [F (if (odd? x)                      ; reactive fns are TitleCase
                              (e/fn [x] (* y x))
                              (e/fn [x] (+ y x)))]
                    (F. x))))                               ; reactive call
    % := 10
    (swap! !x inc)
    % := 12
    (swap! !y inc)
    % := 13
    (swap! !x inc)
    % := 33
    (swap! !y inc)
    % := 36))

(tests
  "reactive for with stabilized body (mount/unmount lifecycle)"
  (def !xs (atom [1 2 3]))
  (with (e/run (tap (e/for [x (e/watch !xs)]   ; p/for is concurrent
                      (tap (- x)))))           ; like React.js we want to stabilize over time
    (hash-set % % %) := #{-2 -1 -3}            ; the for branches are visited concurrently
    % := [-1 -2 -3]                            ; the end result is ordered
    (swap! !xs conj 4)
    % := -4                                    ; only the new item is visited
    % := [-1 -2 -3 -4]))

#?(:clj
   (tests
    "p/for is concurrent like pmap"
    (mapv #(tap (inc %)) (range 3))
    [% % %] := [1 2 3]
    (pmap #(tap (inc %)) (range 3))
    (hash-set % % %) := #{2 1 3}))

(tests
  "reactive exceptions"
  (e/defn Boom [x]
    (if (even? x)
      (throw (ex-info "" {}))))

  (def !x (atom 0))
  (with (e/run (tap (try
                    (Boom. (e/watch !x))
                    42
                    (catch #?(:clj Exception, :cljs :default) e
                      ::exception))))
    % := ::exception
    (swap! !x inc)
    % := 42))
