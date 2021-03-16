(ns dustin.dataflow-leo
  (:require [hfdl :refer [dataflow]]
            [minitest :refer [tests]]
            [missionary.core :as m]))


; What is clojure compatibility?
; Expect from HFDL to be able to use existing macros including clojure.core macros, destructuring
; if use destructure in let, desugar to get/next/first
; s-expressions and ordinary let bindings
; expect this to work incrementally – if the input changes, then the result will be propogated to the apply node
; that computes the next value (call to next). Want the compiler to figure out that next is clojure.core fn , it's a
; constant, it can be optimized



(declare dataflow df-compile)

(tests
  (def a! (atom 0))
  (def a (m/watch a!))
  (def program (dataflow (inc @a)))

  (df-compile program)
  := '(m/signal! (m/latest inc a))
  )


; Geoffrey-lang
(tests
  (def a! (atom 0))
  (def a (m/watch a!))
  (def program (dataflow (inc @a)))

  (df-compile program)
  := '(m/signal! (m/latest inc a))
  )

; Missionary is the right physical vm model to implement this


