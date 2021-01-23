(ns dustin.trace12
  (:require [minitest :refer [tests]]
            [missionary.core :as m :refer [ap ?? ?! ? reactor stream! signal!]]
            [hyperfiddle.incremental :as I :refer [incr?]]))


(tests
  "old worldview"

  ; Splitting this introduces accidental complexity
  (def ast1 '(let [>b (vector ~>ui)]))
  (def ast2 '(let [>ui (inputI)
                   >c (vector ~>b)]))                       ; note >b depends on >ui but in a different reactor

  ; [x] AST must reflect that some nodes have a different behavior depending on server or client
  (declare !client-reactor !server-reactor)
  (def !server-reactor (->Reactor ast1 {!client-reactor '{>ui >ui}}))
  (def !client-reactor (->Reactor ast2 {!server-reactor '{>b >b}}))

  ; >b is passive on the client, active on server
  ; >c is active on client, not defined on server
  )


(tests
  "new way, passive/active marked by reader conditionals"

  (def ast '(let [>a (inputI)
                  >b (#?(:clj vector :cljs nil) ~>a)
                  >c (#?(:clj nil :cljs vector) ~>b)]))

  (def !server-reactor (->Reactor ast))
  (def !client-reactor (->Reactor ast))

  )

(tests
  "passive/active marked by reactor constructor"
  ; Public DSL for distributed computations
  (def ast '(let [>a (inputI)
                  >b (vector ~>a)
                  >c (vector ~>b)]))

  ; [x] AST must reflect that some nodes have a different behavior depending on server or client
  (def !server-reactor (->Reactor ast))
  (def !client-reactor (->Reactor ast {!server-reactor #_#{'>b}}))

  (directive! !server-reactor '(put >a 1))

  (replay! !client-reactor ['(pulse >b [1])])
  (log !client-reactor) := ['(pulse >b [1])
                            '(pulse >c [[1]])]
  )

(tests

  (def ast '(let [>ui (inputI)
                  >b (vector ~>ui)
                  >c (pr-str ~>b)]))

  (declare !client-reactor !server-reactor)
  (def !server-reactor (->Reactor ast {!client-reactor #{'>ui}}))
  (def !client-reactor (->Reactor ast {!server-reactor #{'>b}}))


  #?(:cljs (directive! !client-reactor '(put >ui 1)))

  (replay! !client-reactor ['(pulse >b [1])])
  (log !client-reactor) := ['(pulse >b [1])
                            '(pulse >c [[1]])]
  )

; Is it important there is a single AST?
; What is important is there is a single source of truth for the naming convention
; Both are possible, question is which is more convenient

; Duplication
; What if a significant part of the application can be computed in both reactors?
; There would be a lot of duplication, because need to define the common part twice
; maybe this is not a problem?

; Concern with old worldview:
; Dag structure of the distributed system is
; we have two lets that must be merged to have the full topology.


;(tests
;  "with bind"
;  (def ast1 '(let [>cross ~(case ~>control' :p >p' :q >q')]))
;  )


; both client and server need to agree on the convention
; bind nodes especially

; one single reactor pair shares an AST

; AST Can be composed


; HATEOS = browser that follows links
; links are offered by server
; links / affordance