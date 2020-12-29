(ns dustin.trace1
  (:require [minitest :refer [tests]]
            [hyperfiddle.hfql19 :refer [sequenceI sequence-mapI bindI pureI fmapI capI joinI
                                        hf-nav]])
  )


(declare with-flow)
(def ^:dynamic *flow*)
(declare inputI nodeI ->Flow)

(tests
  "a flow is an abstract DAG"
  "This is a static flow"
  (def >a (pureI 1))
  (def >b (fmapI inc >a))
  (def >z (fmapI vector >a >b))
  (capI >z) := [1 2]

  "flows are represented by AST and created by free monad"
  (def ast1
    '(let [>b (fmapI inc >a)
           >z (fmapI vector >a >b)]))

  "the AST monad operations can be implicit (free monad)"
  (def ast2
    '(mlet [b (inc a)
            z (vector a b)]))

  "do-notation"
  (def ast3
    '(let [>b (inc ~>a)
           z ~(vector ~>a ~>b)]))

  "nodes have internal identity in the flow, so we do not need to use
  namespace state"
  (def flow1 (->Flow))                                       ; flow state
  (binding [*flow* flow1]                                    ; accumulates node defs
    (let [>a (nodeI)                                        ; abstract node that we don't know anything about, defined somewhere else
          >b (fmapI inc >a)
          >z (fmapI vector >a >b)]))
  ; can throw away name >z, it is accumulated in the flow with an internal id

  "vizualize the 'dry' flow (no values puled through yet)"
  "Flows are a value, otherwise we cannot reconstruct it"
  "a flow is a network and you can look at and see its flowchart structure"
  (viz flow1) := '{:edges    {0        nil                         ; a
                              1 #_fmap inc                  ; b
                              2 #_fmap vector}              ; z = fmap
                   :topology #{[0 -> 1]                     ; b = fmap inc a
                               [0 -> 2] [1 -> 2]}           ; z = fmap vector a b
                   :dag {}
                   :state    {}} ; no memoized values yet

  "run water through flow (this is a static flow)"
  (put (get flow1 0) 42)
  ; is it eager or lazy?
  ; eager means auto-stabilizing and always up to date
  (viz flow1) := '{:edges    {0 nil
                              1 inc
                              2 vector}
                   :topology #{[0 -> 1]
                               [0 -> 2] [1 -> 2]}
                   :dag {}
                   :state    {0 42                          ; a
                              1 43                          ; b = inc a
                              2 [42 43]}}                   ; z = vector a b

  "This is a self-adjusting flow"
  (defn ->Flow [ast] (eval ast))
  (def ast '(let [>p (nodeI)                                        ; no inputs, one output edge, can still resume a value
                  >q (nodeI)
                  >control (nodeI)
                  >cross (bindI >control (fn foo [c]
                                           (case c :p >p :q >q)))
                  >z (fmapI vector #_#_>p >q >cross)]))
  (def flow1 (->Flow ast))













  (def ast '(let [>p (nodeI)                                        ; no inputs, one output edge, can still resume a value
                  >q (nodeI)
                  >control (nodeI)
                  >cross (bindI >control (fn foo [c]
                                           (case c :p >p :q >q)))
                  >z (fmapI vector #_#_>p >q >cross)]))

  trace := '[(put >control :q)                              ; input
             (resume >conrol :q)                           ; output

             ; these are wormhole outputs, from some other flow/trace
             ; only the ouput is visible here
             (resume >p 1)
             (resume >q 2)

             (resume >cross 2)                              ; output at wormhole
             (resume >z [2])
             ]

  (defn ->Flow [ast & traces]
    (eval ast)
    (when traces (for [trace traces] (run-effects! trace)))
    *this)

  (def flow0 (->Flow ast))                                   ; no trace
  ; no values yet
  (add-watch flow0 #(swap! !log conj))

  (.execute flow0 '(put >control :q))
  ;(put (get *flow* '>control) :q)
  @!log := ['(put >control :q)
            '(resume >control :q)]


  (def dag (->Flow ast trace))
  (viz dag) := ...

  (def dag' (->Flow ast trace ...))
  (viz dag') := ...


  ; the flow is responsible for producing the trace
  (add-watch !flow)















  (binding [*flow* flow1]
    (let [>p (nodeI)
          >q (nodeI)
          >control (nodeI)
          >cross (nodeI >control ...)
          >z (nodeI >cross ...)]))
  ; (viz flow1) := can never see q connect to z or anything at all


  ; we've changed notation here, need node type for bind trace
  (viz flow1) := '{:edges    {0 node                         ; p
                              1 node                         ; q
                              2 node                         ; control
                              3 (bindI foo)                  ; cross
                              4 (fmapI vector)               ; z
                              }
                   :topology #{#_[_ -> 0] #_[_ -> 1] #_[_ -> 2] ; redundant
                               [2 -> 3]                      ; control -> cross
                               [3 -> 4]}                     ; cross -> z
                   :dag     {3 ?}
                   :state   {}}
  ; cannot have state for z-4 until cross-3 is bound

  ; Trace can effect :state and effect :dag
  ; can it also effect 'put ?

  ; what is difference between put and resume?
  ; resume sets the state. "This happened here at time T"
  ; put is the effect of taking a value from environment and firing
  ; put is source - take from env and put into flow
  (resume (get flow1 2) :q)
  (resume (get flow2 0) 1)
  (resume (get flow2 1) 2)

  (resume >control :q)
  (pulse >control :q)                    ; pulse the output edge of control
  (put >control :q)                     ; pulse the output edge of this input
  ; put is just resume, but restricted to user-writable "input" nodes

  trace := '[(put >control :q)                              ; input
             (resume >control :q)                           ; output
             (put >p 1)
             (put >q 2)
             (resume >q 2)
             (resume >cross 2)                              ; output at wormhole
             ]
  ; we know we don't need to trace puts
  ; what does put bring that resume doesn't? the useless input nodes
  '---------------------------------------------------------------

  ; Since this flow is self-adjusting,
  ; the dag adjusts over time, we must account the time dimension


  ; bind is a pointer to another flow (another dimension)
  ; bind encapsulates the wormhole
  ; q -> z do not have a logical connection

  (viz flow) := '{:edges    {0 node                         ; p
                             1 node                         ; q
                             2 node                         ; control
                             3 (bindI foo)                  ; cross
                             4 (fmapI vector)               ; z
                             }
                  ; seperate flow?
                  :flow #{                              ; -- [1 meta-> 4] ; q -> z - WRONG
                          [2 -> 3]                      ; control -> cross
                          [3 -> 4]                      ; cross -> z
                          }
                  ;:dag #{[1 -> 4]}
                  :dag    {3 1}                        ; connect wormholes
                  :state    {0 1
                             1 2
                             2 :q                           ;2 :falure
                             ;3 #wormhole 2 ; redundant
                             4 [2]}}

  "Can you have two flows? Yes and they compose"
  "flows compose and can concat"
  "flows can pulse"
  "a flow's structure can evolve (self-adjust)"


  "can reconstruct a flow from a trace"

  )


; we are not looking at water, just the riverbed


; what is an ast?
; what is a flow?
;    the flow is the pipes
; what is a dag?
;    a dag is the water

; what is a snapshot
;    both pipes and water in pipes ?
; what is a trace
; are they the same thing?



; you can reconstruct the symbolic names
; by replaying the AST with a trace

; if no bind, a flow and a dag are the same thing
; which do you trace, a flow?



(tests


  trace := '[(declare >a)
             (def >b (bind >a pure))]
  (def flow)

  (viz flow) := '{:edges {0 nil
                          1 (bind pure)}
                  :topo  #{[0 -> 1]}
                  :dag   {}
                  :state {}}

  trace := ['(resume 0 :foo)]
  trace := ['(declare 2)
            '(resume 1 2)]
  flow ...

  (viz flow) := '{:edges {0 nil
                          1 (bind pure)
                          2 nil}
                  :topo  #{[0 -> 1]}
                  :dag   {1 2}
                  :state {0 :foo
                          2 :foo
                          #_#_1 :foo                        ; redundant
                          }}

  ; replay again
  trace := '[(declare >a)
             (def >b (bind >a pure))]
  trace := ['(resume 0 :bar)]
  trace := ['(declare 2)
            '(resume 1 2)]

  ; pure needs to understand react keys
  ; node ids are react keys
  ; ids are concatenable recursively (paths)
  (viz flow) := '{:edges {0 nil
                          1 (bind pure)
                          ;2 nil                            ; gc'ed
                          3 nil}
                  :ast  #{[0 -> 1]}
                  :dag   {1 3}
                  :state {0 :bar
                          ;2 :foo ; gc'ed
                          3 :bar
                          }}
  ;[1 <- 0]

  )

(tests
  ; third time
  trace := '[(declare >a)
             (def >b (bind >a pure))]
  trace := ['(resume 0 :bar)]
  trace := ['(declare 2)
            '(resume 1 2)
            #_'(resume-bind 1 3)]

  (def f pure)

  ; pure needs to understand react keys
  ; node ids are react keys
  ; ids are concatenable recursively (paths)
  (viz flow ast & traces)
  := '{:edges {0 nil
               1 (bind 'f)
               ;2 nil                            ; gc'ed
               3 nil}
       :ast   #{[0 -> 1]}
       :binds {1 3}
       :state {0 :bar
               ;2 :foo ; gc'ed
               3 :bar
               }}

  ; produce trace = do effects
  ; yes, flow contains fns even when moved, to extent possible
  ; fns can be symbolic (resolve into ns)

  ; if replaying from trace, don't care about node types ?!
  ; when replaying a trace, do we even see the wormholes?
  ; yes the point of the trace is to get at the intermediate nodes

  ; if huge collection, every time parent node puleses and repro same result
  ; if there is only small delta, resend full collection over wire as part of trace

  ; ast
  ; node types, functions (symbols), node names

  ; flow
  ; - does not need to embed the AST if coordinated out of band

  ; trace
  ; - does not need to embed node types if coordinated out of band through ast



  )

