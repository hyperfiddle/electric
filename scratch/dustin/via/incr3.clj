(ns dustin.via.incr3
  (:require
    [clojure.walk :refer [postwalk]]
    [dustin.via.free3 :as free :refer [fapply join pure inject run boot bind if2
                                       lift-and-resolve]]
    [meander.epsilon :refer [match]]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]
    [missionary.core :as m]))


; Goal is to figure out reactive rendering with dynamic control flow (not templates)
; as well as clojure interop for if-statements

(tests
  "reagent approach (bad) - dynamically render a static template
  the template now needs react-keys to reconnect the supervisor pointers
  what's good is, for and if are clojure/if and clojure/for"
  (def !result (atom []))
  (def !xs (atom [0 1 2])) (def xs (m/watch !xs))
  (def !c (atom :odd)) (def c (m/watch !c))
  (def !d (atom :even)) (def d (m/watch !d))

  "reagent hiccup syntax
  bad – no reactive control flow"
  (defn App1 [xs]
    [table                                                  ; hiccup is incremental
     (let [n (count xs)]                                    ; clojure interop - build hiccup template
       [span n])
     (for [x xs]
       (let [>e (d/entity-incr :db/id x)]
         [tr (if (odd? x)
               [div >e c x [span c x]]
               [div >e d x [span d x]])]))])

  "same thing with clojure syntax templates
  bad – no reactive control flow"
  (defn App2 [xs]
    `(table
       ~(let [n (count xs)]
          `(span ~n))
       ~(for [x xs]
          (let [>e (d/entity-incr :db/id x)]
            `(tr ~(if (odd? x)
                    `(div ~>e ~c ~x (span ~c ~x))           ; x is a value literal which will lift
                    `(div ~>e ~d ~x (span ~d ~x))))))))

  (boot (Eval-incr {`log (partial log! !result)})
    `(log (App2 ~xs))))

; Need a bind symbol to disambiguate interop (odd? x) from incr (table (tr ...))
; (new table ...) gives the right intuition, this is a heap object persistent over time
; and you pass messages to it
