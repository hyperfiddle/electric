(ns dustin.trace6
  (:require [minitest :refer [tests]]
            [hyperfiddle.incremental :as I :refer [incr?]]))

(declare flow1 viz ast1 ->Flow compile-incr ast2 flow2 Flow
  dag1 dag2 directive! replay! !log1 !log2)

; Produce a snapshot of a flow – clearly showing internal as EDN
; Recreate a flow from a snapshot

; Trace5 we learn that the Dataflow-AST is part of the snapshot
; What is a dataflow AST
;  it is a DAG with no closures. Bind nodes are different from "closure nodes"
;     by having explicitly marked inputs. Bind node has inputs, closure has stack frame
;     lexical environment is a stack of frames
;     This is difference between dataflow and continuations
; Is that different than a clojure AST that defines a dataflow? YES

(tests

  ; via notation
  (def ast1 '(let [>p (inputI)
                   >q (inputI)
                   >control (inputI)
                   #_#_>control (identity ~>control)]))
  ; inputs and outputs must be reachable

  (def ast2 '(let [>cross ~(case ~>control :p >p :q >q)
                   >z (vector ~>cross)
                   >z (identity ~>z)]))                     ; shadow

  (def flow1 (Flow ast1 {}))
  (def flow2 (Flow ast2 {'>p       (get flow1 '>p)
                         '>q       (get flow1 '>q)
                         '>control (get flow1 '>control)}))

  (def !log1 (atom [])) (add-watch flow1 ::flow1 #(swap! !log1 conj))
  (def !log2 (atom [])) (add-watch flow2 ::flow2 #(swap! !log2 conj))

  (directive! flow1 '(put >control :q))

  @!log1 := ['(pulse >control :q) '(pulse >cross >q)]
  @!log2 := ['(pulse >control :q) '(pulse >cross >q)]

  (def dag1 @flow1)
  (viz dag1) := {:ast   ast1
                 :nodes '{>p       {:type input}
                          >q       {:type input}
                          >control {:type input :val :q}}}

  (def dag2 @flow2)
  (viz dag2) := {:ast   ast2
                 :nodes '{>cross   {:type bind :val '>q}
                          >z       {:type fmap}
                          >p       {:type ?} #_NodeDef
                          >q       {:type ?}
                          >control {:type ? :val :q}}}

  (replay! flow2 ['(pulse >p 1)
                  '(pulse >q 2)
                  '(pulse >z [2])])

  (viz @flow2) := {:ast   ast2
                   :nodes {'>cross   {:type 'bind :val '>q}
                           '>z       {:type 'fmap :val [2]}
                           '>p       {:type '? :val 1}
                           '>q       {:type '? :val 2}
                           '>control {:type '? :val :q}}}

  )

