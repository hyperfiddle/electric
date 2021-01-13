(ns dustin.trace10
  (:require [minitest :refer [tests]]
            [missionary.core :as m :refer [ap ?? ?! ? reactor stream! signal!]]
            [hyperfiddle.incremental :as I :refer [incr?]]))

(declare
  !server-reactor
  !client-reactor
  add-watch
  ->Reactor
  eval-incr ... IWatch
  flow1 viz ast1 ->Flow compile-incr ast2 !log2 Flow
  dag1 dag2 directive! replay! !flow1 !flow2)

(deftype ReactorImpl []
  IWatch
  IDeref)

(defn Reactor [ast connections]
  (reactor
    (let [!log (atom [])
          _ (eval-ast-to-self-adjusting-graph ast)
          reactor-input ...]
      (subscribe-to-connections (keys connections))
      (m/stream!
        (ap (swap! !log conj (?? reactor-input))))

      (reify
        IDeref
        (deref [o])
        Trace
        (directive! [o])                                    ; send inputs to reactor
        (subscribe! [o])
        (log [o] @!log)
        (replay! [o log])
        ))))                                                ; emit effects that reactor propogates

(tests

  (def ast1 '(let [>x (pure 10)   ; programmer defined constant, compiler can inline it
                   >a (inputI)
                   >p (inputI)
                   >q (inputI)
                   _ (pure 2)     ; programmer says anonymous

                   >control (inputI)  ; shadowed by programmer, on purpose
                   >control (vector (f 1) ~>a)]))

  (mlet [>x (pure 10)   ; programmer defined constant, compiler can inline it
         >a (inputI)
         >p (inputI)
         >q (inputI)
         _   (pure 2)     ; programmer says anonymous

         >control (inputI)  ; shadowed by programmer, on purpose
         >control (vector a)])

  (def !server-reactor (->Reactor ast1 {}))

  ; data Effect = Pulse n v tx | Insert | Remove | Throw | Done
  ; at T0, >control's output edge succeeded with value :q


  ; Second reactor in sync

  (def ast2 '(let [>simple (vector ~>control')
                   >cross ~(case ~>control' :p >p' :q >q')
                   >z (vector ~>cross)
                   >z (identity ~>z)]))                     ; shadow

  (def ast2' '(let [>simple (vector (?! >control'))
                    >cross (?! (case (?! >control') :p >p' :q >q'))
                    >z (vector (?! >cross))
                    >z (identity (?! >z))]))

  (def !client-reactor (->Reactor ast2 {!server-reactor '{>p'       >p
                                                          >q'       >q
                                                          >control' >control}}))

  (directive! !server-reactor '(put >a :q))                 ; cause (input)
  ; put is the same as reset! an atom
  ; one directive triggers a propogation, the result of the propogation
  ; is a set of pulses

  ; same propogation, turn, frame, tier, tick, tx
  (log !server-reactor) := ['(pulse >a :q t0)
                            '(pulse >control :q t0)         ; effect = success
                            #_'(pulse >xs [1 2 3 4 5 ... 99 100] t1)]

  @!server-reactor := {:ast   ast1
                       :nodes '{>p       {:type input}
                                >q       {:type input}
                                >control {:type input :val :q}}}

  (log !client-reactor) := ['(pulse >control :q t0)
                            '(pulse >simple [:q] t0)
                            ;'(pulse >cross ~>q')

                            ]                               ; >a is dropped

  ; data NodeType = Derived | Input | Bind

  @!client-reactor := {:ast   ast2
                       :nodes '{>simple   {:type derived :val [:q]}
                                >cross    {}     ; how to represent topology changes?
                                >z        {}
                                >p'       {:type ?}
                                >q'       {:type ?}
                                >control' {:type ? :val :q}
                                }}

  ; Unclear what NodeType purpose is
  ; helps with debugging of course
  ; probably needed for change in topology
  ; Known in advance by both server and client because derived from HFQL

  )
