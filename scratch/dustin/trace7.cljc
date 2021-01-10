(ns dustin.trace7
  (:require [minitest :refer [tests]]
            [hyperfiddle.incremental :as I :refer [incr?]]))

(declare flow1 viz ast1 ->Flow compile-incr ast2 !log2 Flow
  dag1 dag2 directive! replay! !flow1 !flow2)

(defn Flow' [])

(deftype Flow []
  IWatch
  IDeref)

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


  ; THe language of the trace and the language of the reactor
  ; encodes all possible dynamic choices
  (def ast2 '(let [>cross (signal! (?! (case (?! >control) :p >p :q >q)))
                   >z (vector (?! >cross))
                   >z (identity (?! >z))]))

  (def ast2 '(let [>cross (signal! (?! (case (?! >control) :p >p :q >q)))
                   >z (vector (?! >cross))
                   >z (identity (?! >z))]))

  ; Topology
  ; The Reactor maintains a graph and the topology of the graph is dynamic
  ; In reaction to any event, you can register a new node,
  ; a running node can subscribe to another node (that will create an edge)
  ; A node can cancel one of its subscription (remove an edge)
  ; a node can terminate (remove the node and all its downstream edges)
  ; A node can crash, this is almost like a termination
  ; A node can be cancelled from the outside
  ; A node that is cancelled is not subscribe-able anymore. If you try, the subscription will fail
  ; What is dynamic?
  ; Stream is like signal, for discrete flows.

  (def !flow1 (Flow ast1 {})) #_#_(def !flow1 (atom [])) (add-watch flow1 ::flow1 #(swap! !flow1 conj))

  (def !flow2 (Flow ast2 {^Watch !flow1 {'>p2      '>p
                                         '>q2      '>q
                                         '>control '>control}}))
  ; at scale, this connection config is like a namespace of defs
  ; with require :refer :as and aliases

  (directive! flow1 '(put >control :q))

  ; You can produce the log yourself by add-watch on !flow1
  ;@!log1 := ['(pulse >control :q) '(pulse >cross >q)]
  ;@!log2 := ['(pulse >control :q) '(pulse >cross >q)]

  (def dag1 @!flow1)                                        ; this is a dag ? But no edges
  @!flow1 := {:ast   ast1                                   ; dag edges are in the AST
              :nodes '{>p       {:type input}
                       >q       {:type input}
                       >control {:type input :val :q}}}

  (def dag2 @!flow2)
  (viz dag2) := {:ast   ast2
                 :nodes '{>cross   {:type bind :val '>q}
                          >z       {:type fmap}
                          >p       {:type ?}
                          >q       {:type ?}
                          >control {:type ? :val :q}}}

  ;(replay! !flow2 ['(pulse >p 1)
  ;                 '(pulse >q 2)
  ;                 '(pulse >z [2])])

  ; pulse is the only effect type, also group by time
  (replay! !flow2 [{'>p 1}
                   {'>q 2}
                   {'>z [2]}])

  (viz @!flow2) := {:ast   ast2
                    :nodes {'>cross   {:type 'bind :val '>q}
                            '>z       {:type 'fmap :val [2]}
                            '>p       {:type '? :val 1}
                            '>q       {:type '? :val 2}
                            '>control {:type '? :val :q}}}

  ; ?
  ;(def !flow2' (hydrate @!flow2))
  ;@!flow2' := @!flow2

  (def !flow3 (Flow ast2 {!flow2 '{>control >control}}))


  )