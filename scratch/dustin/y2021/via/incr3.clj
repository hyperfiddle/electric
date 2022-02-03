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

(declare entity-incr table span tr div Eval-incr)

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
       (let [>e (entity-incr :db/id x)]
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
          (let [>e (entity-incr :db/id x)]
            `(tr ~(if (odd? x)
                    `(div ~>e ~c ~x (span ~c ~x))           ; x is a value literal which will lift
                    `(div ~>e ~d ~x (span ~d ~x))))))))

  (boot (Eval-incr {`log (partial log! !result)})
    `(log (App2 ~xs))))

; Need a bind symbol to disambiguate interop (odd? x) from incr (table (tr ...))
; (new table ...) gives the right intuition, this is a heap object persistent over time
; and you pass messages to it

(tests
  (def !result (atom []))
  (def !xs (atom [0 1 2])) (def xs (m/watch !xs))
  (def !c (atom :odd)) (def >c (m/watch !c))
  (def !d (atom :even)) (def >d (m/watch !d))

  "ast with native dynamic control flow, interpreted incrementally"
  {table. (lift table)}                                     ; incremental env must be lifted

  ; . indicates which apply(bind) you want: host or dsl.
  ; (f. ...) indicates a bind with variable arity (bundles the join)
  ; dsl apply is entirely lifted ; whole sexpr is lifted including fncall.
  ; host apply is exposed to monadic types and therefore must fuse through cloroutine
  (defn App [xs]
    `(table.                                                ; dsl apply
       (for. [>x xs]                                        ; >x signal has bound identity
         (let [>e (entity-incr. :db/id >x)]                 ; >e conj's bound identity stack
           (tr. (if. (true? (odd? @(:user/age. >e)))        ; @ is for dsl->host interop (all interop goes through cloroutine)
                  (div. >e >c '(count xs) (span. >c))       ; ' is for host->dsl interop (lifting)
                  (div. >e >d '(count xs) (span. >d))))))
       (let [n (count xs)]                                  ; host let, host apply
         (span. 'n))))

  (boot (Eval-incr {`log (partial log! !result)
                    `table (lift table) ...})
    `(log (App ~xs)))
  )

(defmacro dag [body]
  ; capture lexical &env; resolve vars in correct scope
  body)

(tests
  (def !result (atom []))
  (def !xs (atom [0 1 2])) (def xs (m/watch !xs))
  (def !c (atom :odd)) (def >c (m/watch !c))
  (def !d (atom :even)) (def >d (m/watch !d))

  "ast with native dynamic control flow, interpreted incrementally"
  {table. (lift table)}                                     ; incremental env must be lifted

  ; . indicates which apply(bind) you want: host or dsl.
  ; (f. ...) indicates a bind with variable arity (bundles the join)
  ; dsl apply is entirely lifted ; whole sexpr is lifted including fncall.
  ; host apply is exposed to monadic types and therefore must fuse through cloroutine
  (defn App [db xs]
    (dag
      (table.                                               ; dsl apply
        (for. [x xs]                                        ; new frames in parallel, x in each frame
          (let. [e (entity. db x)                           ; dsl let, which achieves simpler types! (host let would expose signal)
                 id (entity-get. x :db/id)]
            (tr. (if. (true? (odd? (inc @(:user/age. e))))  ; @ is for dsl->host interop (all interop goes through cloroutine)
                   (div. id >c '(count xs) (span. >c))      ; ' is for host->dsl interop (lifting)
                   (div. id >d '(count xs) (span. >d)))
              (let [ks (entity-keys. x)]                    ; >e conj's bound identity stack
                (pre. (pr-str @(sequence (map #(entity-get. x %) ks))))))))
        (let [{n :count} {:count (count xs)}]               ; host let, host apply
          (span. 'n)))))

  (boot (Eval-incr {`log (partial log! !result)
                    `table (lift table) ...})
    (dag (log. (App. xs))))
  )

(tests

  "ast with native dynamic control flow, interpreted incrementally"
  (defn App2 [xs]
    `(table.
       (let [n (count xs)]
         (span. 'n))
       (for [x xs]                                         ; clojure for ???
         (let [>e (entity-incr. :db/id x)]
           (tr. (if. (odd? (:user/age x))                   ; type error
                  (div. >e >c 'x (span. >c 'x))
                  (div. >e >d 'x (span. >d 'x))))))))

  "inverted polarity"
  (defn App2 [xs]
    `(table.
       (let [n (count xs)]
         (span. 'n))
       (for. [>x xs]
         (let [>e (entity-incr :db/id @>x)]
           (tr. (if. (odd? @>x)
                  (div. @>e @c @>x (span. @c @>x))
                  (div. @>e @d @>x (span. @d @>x))))))))

  )