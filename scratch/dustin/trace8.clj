(ns dustin.trace8
  (:require [minitest :refer [tests]]
            [hyperfiddle.incremental :as I :refer [incr?]]
            [missionary.core :as m]))


(declare hfql submissions shirt-sizes genders needle *visitor*)


; Trace itself
; Notation for flow


; Goal
;


(defn hfql [pull-query scope]

  )

(def !needle (atom nil))


(binding [*tree-transformer* #?(:clj  tree-edn-render
                                :cljs hiccup-ui)]
  (hfql
    [{(submissions needle)
      [:dustingetz/email
       {:dustingetz/gender
        [:db/ident
         {(shirt-sizes gender)
          [:db/ident]}]}]}
     {(genders) [:db/ident]}]
    {'needle (m/watch !needle)}))

(reset! !needle "alice")

:=
'{(submissions needle) [#:dustingetz{:email  "alice@example.com",
                                     :gender {:db/ident                       :dustingetz/female,
                                              (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/womens-small}
                                                                               #:db{:ident :dustingetz/womens-medium}
                                                                               #:db{:ident :dustingetz/womens-large}]}}
                        ],
  (genders)            [#:db{:ident :dustingetz/male} #:db{:ident :dustingetz/female}]}


(defn Flow [ast connections]
  ...)

(declare directive! ast1 ast2 !flow1 !flow2)


(tests
  (def ast1 '(let [>p (m/watch (atom nil))
                   >q (m/watch (atom nil))
                   >control (m/watch (atom nil))]))

  ; Flow is a graph whose topology evolves over time



  (def ast2 '(let [>cross (bind >control (fn [control]
                                           (case control
                                             :p >p2
                                             :q >q2)))
                   >z (fmap vector >cross)]))

  ;(def ?! ~)
  ; continuous flow, also known as behavior
  (def mflow2 '(cp (let [>cross (signal! (?! (case (?! >control) :p >p2 :q >q2)))
                         >z (signal! (?! (vector (?! >cross))))]
                     ... >z ...)))
  (def reactor2 (eval mflow2))

  (new-hfql
    {(submission needle)
     [:dustingetz/gender]})
  := '(cp (let [>>xs (extend-seq (submissions (?! needle)))]
            (for [>x (?! >>xs)]
              (let [y (hf-nav :dustingetz/gender (?! >x))]

                ))
            {'(submission needle) {:dustingetz/gender y}}))


  (macroexpand-1
    '(hfql
       [{(submission needle)
         [:dustingetz/gender]
         #_[{:dustingetz/gender
             [{(shirt-size gender)
               [:db/id :db/ident]}]}]}]))
  := '(cp (let [>cross (signal! (?! (case (?! >control) :p >p2 :q >q2)))
                >z (signal! (?! (vector (?! >cross))))]
            ... >z ...))



  (def !flow1 (Reactor ast1 {}))

  ; The graph is aware of its static inputs
  (def !flow2 (Flow ast2 {!flow1 {'>p2      '>p
                                  '>q2      '>q
                                  '>control '>control}}))

  ; Directive is a way to send message to an input (and only input)
  (directive! !flow1 '(put >control :q))                    ; cause

  ; Effect is internal state changes to the flow
  @!flow1 := ['(pulse >control :q)]                         ; effects

  (def dag @!flow1)                                         ; difference between dag and flo
  (viz-edn dag) := {:ast   ast1                             ; dag edges are in the AST
                    :nodes '{>p       {:type input :val nil}
                             >q       {:type input :val nil}
                             >control {:type input :val :q}}}

  (directive! !flow1 '(put >control :p))
  @!flow1 := ['(pulse >control :q)
              '(pulse >control :p)]

  (def dag @!flow1)                                         ; difference between dag and flo
  (viz-edn dag) := {:ast   ast1                             ; dag edges are in the AST
                    :nodes '{>p       {:type input :val nil}
                             >q       {:type input :val nil}
                             >control {:type input :val :p}}}

  (def !flow3 (Flow ast1))
  (viz-edn @!flow3) := {:ast   ast1
                        :nodes {}}

  (replay! !flow3 ['(pulse >control :q)
                   '(pulse >control :p)])

  (viz-edn @!flow3) := {:ast   ast1
                        :nodes '{>p       {:type input :val nil}
                                 >q       {:type input :val nil}
                                 >control {:type input :val :p}}}

  )

; what is a flow?
; a dag is a flow at a point in time


; From Geoffrey - trace8
(tests
  (def ast '(let [>items (inputI)
                  >items2 (fmap #(map inc %) >items)
                  >>extended (extend-seq :db/id >items)]))
  (def flow (->Flow ast))

  (directive! flow '(put >items [{:db/id 1} {:db/id 2}]))

  @flow := ['(pulse >items [{:db/id 1} {:db/id 2}])
            '(pulse #uuid "…00" {:db/id 1})                 ; advertise new nodes to client
            '(pulse #uuid "…01" {:db/id 2})]

  ; Need directive to add and remove nodes

  (directive! flow '(put >items [{:db/id 1} {:db/id 2} {:db/id 3}]))
  @flow := [...
            '(pulse #uuid "…02" {:db/id 3})]

  (directive! flow '(put >items [{:db/id 1} {:db/id 4} {:db/id 2} {:db/id 3}]))
  @flow := [...
            '(pulse #uuid "…03" {:db/id 4})]                ; in what order


  ;(viz @flow) := {:ast   ast
  ;                :nodes {'>items     {:type '? :val [{:db/id 1} {:db/id 2}]}
  ;                        #uuid "…00" {:type 'fmap :val {:db/id 1}}
  ;                        #uuid "…01" {:type 'fmap :val {:db/id 2}}
  ;                        '>>extended {:type 'fmap :val [#ref #uuid "…00", #ref #uuid "…01"]}}}

  )

; in missionary a flow is a recipe for creating a node
; a recipe is a description of the behavior
; a node is a behavior associated with an identity
; flows are what RX calls observable
; RX has hot and cold. Cold is just a recpie. Hot is backed by a stable identity.

; We need to understand RX hot/cold

; m/signal! takes a flow that returns a flow with an identity
; what we call a flow, Leo calls a running instance of reactor

; in missionary, a reactor can spawn a new node from an arbitrary behavior
; the node behavior is not part of the topology


