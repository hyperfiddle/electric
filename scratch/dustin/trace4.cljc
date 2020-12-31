(ns dustin.trace4
  (:require [minitest :refer [tests]]
            [hyperfiddle.incremental :as I :refer [incr?]]))

(declare directive! replay! compile-incr)

(deftype Flow [ast inbound-edges registry]
  ILookup
  (get [o k] (get registry k)))

(defn Flow [ast inbound-edges]
  (->Flow ast (to-weakmap inbound-edges)
    (binding [*registry* (new HashMap)]
      (eval (compile-incr ast))
      *registry*)))

(tests
  (compile-incr '(fmap identity >a)) := '(fmapI '>a identity >a) ; concrete monad and retain symbolic names
  (eval *1) :? incr?)

(tests

  (def ast1                                                 ; server
    '(let [>p (inputI)
           >q (inputI)
           >input (inputI)
           >control (fmapI identity >input)]))

  (def ast2                                                 ; shared client/server
    '(let [>cross (bindI >control2 (fn foo [c]
                                    (case c :p >p2 :q >q2)))
           >z (fmapI vector #_#_>p >q >cross)]))

  (def flow1 (Flow ast1 {}))
  (get flow1 '>p) := (inputI)                               ; the node ref the AST produces

  (def flow2 (Flow ast2 {'>p2 (get flow1 '>p)
                         '>q2 (get flow1 '>q) #_ (pureI 2)
                         '>control2 (get flow1 '>control)}))

  (get flow2 '>p2) := (get flow1 '>p)                       ; connected flows
  (get flow2 '>q2) := (get flow1 '>q)
  (get flow2 '>control2) := (get flow1 '>control)

  (def !log1 (atom [])) (add-watch flow1 ::flow1 #(swap! !log1 conj))
  (def !log2 (atom [])) (add-watch flow2 ::flow2 #(swap! !log2 conj)) ; not interesting

  (directive! flow1 '(put >input :q))
  (directive! flow1 '(put >p 1) '(put >q 2))

  @!log1 := ['(pulse >input :q)
             '(pulse >control :q)
             #_'(pulse >cross >q)                            ; not in this flow
             '(pulse >p 1)
             '(pulse >q 2)
             #_'(pulse >z [2])]                              ; not in this flow

  @!log2 := ['(pulse >control2 :q)                           ; i depend on this and it happened
             '(pulse >cross >q)
             '(pulse >p2 1)
             '(pulse >q2 2)
             '(pulse >z [2])]

  (directive! flow1 '(put >input :p))
  @!log1 := [...
             '(pulse >input :p)
             '(pulse >control :p)]
  @!log2 := [...
             '(pulse >control2 :p)
             '(pulse >cross >p2)                            ; implicitly detaches >q2 from flow2
             '(pulse >z [1])]



  ; shadow node names?
  ; shadowing seems to do the right thing , programmer shadowed on purpose those nodes are private
  (def ast3                                                 ; shared client/server
    '(let [>cross (bindI >control2 (fn foo [c]
                                     (case c :p >p2 :q >q2)))
           >z (fmapI vector >cross)
           >z (fmapI vector >cross)]))

  (get (Flow ast3 {}) '>z) := (inputI)                               ; lookup is by Applicative order, latest
  ; this is the right thing!

  ; what is the name of anonymous pure nodes?
  ;(bindI pureI)
  ; we think, like shadowing, you never want a reference to it

  ; unsequence today calls pureI but should not, will use react-keys
  ; see contrib.reactive/unsequence

  ; symbol -> id resolution?
  ; no need, registry handles this

  ; TODO trace ordering, async

  )