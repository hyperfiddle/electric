(ns dustin.dataflow-via
  (:require [hfdl :refer [dataflow]]
            [minitest :refer [tests]]
            [missionary.core :as m]))


(def program '(let [a (inc b)] a))


;(eval program {'b (resolve 'b *ns*)})
;(eval-df program {'b (m/watch (atom 42))})

(compile-df program)
:= ['(let [b (m/signal! b)
           a (m/signal! (m/latest inc b))]
       a)
    {'b 0 'a 1}]

(defn debug! [program]
  (_ (compile program)))

(def process (debug! program))
@process := {:state :running                                ; because the watch introduced variability
             :slots {'b 42
                     'a 43
                     '% 43}
             :sourcemap {'b 0                               ; resolve debug symbols to reactor heap pointer
                         'a 1}}

; eval same program as clojure
(let [b 42] (lexical-eval program))
:= 43


; Clojure with changed evaluation rules
; two types of rules
; 1) stackwise or queuewise?
; 2) the monad

(via (reify IEvaluator
       Traverse ...                                         ; what is the order in which the node are visited? (Naively or with planning)
       Monad ...
       )
  program)

; Traverse is - in what order do you treeseq the nodes? (Because hte hardware is single threaded)
; If RT, it doesn't matter what order, doesn't matter if you memoize
; If not RT, the order of effects matters a lot and you cannot memoize anything.

; Monads
; Linearity vs parallelism




(@f @a)

(def program '(inc a))
:clojure  '(inc a)
:dataflow '(m/latest apply inc [a])

; dataflow
(def evaluator (reify
                 IMonad
                 (pure [c] (Incr c))
                 (fmap [] _)
                 (apply [] _)
                 (bind [] _)))
(via df-evaluator program {'a (pure 42) 'inc (pure inc)})

(def !f (atom inc))
(via df-evaluator program {'a (pure 42) 'inc (m/watch !f)})


; clojure
(compile program {'a 42
                  'inc `inc})
:= '(let [~@env] ~program)






; questions

; Leo-lang is biased towards foreign interop

(compile-leo '(inc @a))
:= '(m/signal! (m/latest clojure.core/inc a))

(compile-dustin '(>inc >a))
:= '(m/signal! (m/latest call >inc >a))
(debug! '(m/signal! (m/latest call >inc >a)) {'>inc (pure clojure.core/inc) '>a _})



(via-1 '(clojure.core/inc ~>a))                          ; ~ means join
:= (via-2 '(~clojure.core/inc >a))                          ; ~ means don't join

:=

'(m/signal! (m/latest call clojure.core/inc >a))
'(fmap clojure.core/inc >a)


:= (pure 2)




;(fapply (pure clojure.core/inc) (pure 1)) := (pure 2)
;(bind (pure 1) (fn [a]
;                 (bind (pure a) (fn [a] (pure (inc a)))))) := (pure 2)


(dataflow

  (defn f [a]
    (inc a))



  )