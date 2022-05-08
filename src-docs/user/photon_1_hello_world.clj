(ns user.photon-1-hello-world
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]))

(comment (hyperfiddle.rcf/enable!))

(tests "hello world"
  (def dispose (p/run (rcf/! ::x)))
  % := ::x
  (dispose))

(tests "react based on a ref"
  (def !x (atom 0))
  (def dispose (p/run (! (p/Watch. !x))))
  % := 0
  (swap! !x inc)
  % := 1
  (dispose))

(tests "dataflow diamond"
  (def !x (atom 0))
  (def dispose
    (p/run
      (let [x (p/Watch. !x)]                                ; one watch, shared
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
    (p/run (! (+ (p/Watch. !x) (p/Watch. !x)))))            ; two watches - bad
  % := 0
  (swap! !x inc)                                            ; each watch fires an event, producing two propagation frames
  % := 1                                                    ; bad
  % := 2
  (swap! !x inc)
  % := 3                                                    ; bad
  % := 4
  (dispose))

(tests "reactive function call"
  (def !x (atom 1))
  (def !f (atom +))
  (def dispose
    (p/run
      (let [f (p/Watch. !f)
            x (p/Watch. !x)]
        (! (f 0 x)))))
  % := 1
  (swap! !x inc)
  % := 2
  (reset! !f -)
  % := -2
  (dispose))

(tests
  "foreign clojure functions including core. map is not incremental, the args are"
  (def !xs (atom [1 2 3]))
  (def !f (atom inc))
  (def dispose
    (p/run
      (! (let [f  (p/Watch. !f)
               xs (p/Watch. !xs)]
           (clojure.core/map f xs)))))
  % := [2 3 4]
  (swap! !xs conj 4)
  % := [2 3 4 5]
  (reset! !f dec)
  % := [0 1 2 3])
