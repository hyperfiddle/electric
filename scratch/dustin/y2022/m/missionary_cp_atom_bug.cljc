(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

(hyperfiddle.rcf/enable!)

(tests
  (def <x (m/cp
            (let [!x (atom 0)
                  x (m/?< (m/watch !x))]
              (if (< x 10)
                (reset! !x (inc x)) x))))
  (def it (<x #() #()))
  @it := 10
  (it))
; ❌ FAIL in () (scratch.cljc:10)
;expected: (= (clojure.core/deref it) 10)
;  actual: (not (= 1 10))

(tests
  "can this be expressed without mutation?"
  (def !y (atom 0))
  (let [<y (m/cp (m/?< <y))] <y)

  (m/eduction (drop 1) >x)

  (let [<y (m/watch !y)
        >y' (m/eduction (drop 1) <y)
        <y' (m/cp (m/?< <y))]
    (reset! !y )
    <y)

  (m/cp (let [<y (m/cp (let [y (m/?< <y)]
                     (if (< y 10)
                       (inc y)
                       y)))])))

(tests
  "sliding window"
  (def >window
    (m/ap
      (let [>x (m/seed [1 2 3])]
        (m/zip vector (m/eduction (drop 1) >x) >x))))

  (def it (>window #(! ::notify) #(! ::terminate)))
  % := ::notify
  @it := [2 1]
  % := ::notify
  @it := [3 2]
  % := ::terminate
  (it))

(tests
  (def dispose ((m/reactor
                  (m/stream!
                    (let [!F (atom (m/cp ::init))]
                      (let [<<x (m/signal! (m/watch !F))
                            <x (m/signal! (m/cp (! (m/?< (m/?< <<x)))))]
                        (m/latest {} <x (m/latest (partial reset! !F) (m/cp <x)))))))
                println println))
  % := ::init
  % := 0
  (dispose))

