(ns dustin.lang-leo
  "tests to communicate about leo lang"

  ; What is our mission ?
  ; Produce test cases that bring the right questions
  ; that make us enquire about design choices

  ; Dustin: Ask questions at .9999999 precision
  ; Dustin: A question at .9 precision does not qualify as a valid question

  ; How will we do it? What artifact will we produce?
  ; create one file lang-leo with 200 LOC test cases
  ; we will test from hello-world to let, if, case, destructuring
  ; what operations introduce signals? We don't know
  ; effects?
  ; reactive-for
  ; Out of scope: ifn, $, nested dags (wait for .99999 precision before moving on to this)

  ; What will D/G do tomorrow? We will read the cases and Dustin will agree that this matches
  ; our understanding of leo-lang and then pass on to Leo

  ; What are we optimizing for?
  ; Not interrupting Leo because he's the severe bottleneck
  ; Build a foundation to communicate on top of



  ; OODA
  ; observe -> orient -> decide -> act ->


  (:require [hfdl :refer [dataflow]]
            [minitest :refer [tests]]
            [missionary.core :as m]))

(tests

  "programs without variability terminate immediately"
  (def program (dataflow 1))
  (def process (debug! program))                            ; (trace!) ?
  @process := {:state  :terminated                          ; Because the program is entirely static
               :result 1                                    ; ??? it terminated with 1
               :vars   {}}

  ; how does shell user ask the entrypoint for the result of a program that maybe terminated?
  ; need a "shell" - C-z, C-c, print result eventually

  "clojure foreign interop with a value"
  (def a 1)                                                 ; value
  (def program (dataflow a))                                ; clojure interop with a value
  (def process (debug! program))
  @process := {:state  :terminated
               :result 1
               :vars   {}}

  "clojure foreign interop to a variable"
  ; variable is not overloaded in clojure so we can take that word
  (def !a (atom 1))
  (def a (m/watch !a))                                      ; HFDL variable (missionary flow)
  (def program (dataflow @a))                               ; clojure interop with a HFDL variable
  (def process (debug! program))
  @process := {:state :running                              ; maintain result forever
               :vars  {'a 1}}                               ; ??? should inbound signals show up in the heap?
                                                            ; GG: thinks yes, the deref call on `a` shows up in the heap.

  "maintain result in response to variable changes"
  (swap ! !a inc)
  @process := {:state :running
               :vars  {'a 2}}

  "Transition between running and terminated state"
  (def !a (atom 0))
  (def a (->> (m/watch !a)
              (m/transform (take-while even?))))
  ​
  (def program (dataflow @a))
  (def process (debug! program))
  ​
  @process := {:state       :running
               :signal-heap {'a 0}}
  ​
  (reset! !a 2)
  ​
  @process := {:state       :running
               :signal-heap {'a 0}}
  ​
  (reset! !a 1)
  ​
  @process := {:state       :terminated
               :result      1
               :signal-heap {}}

  "clojure foreign fn call"
  (def f inc)
  (def program (dataflow (f 1)))
  (def process (debug! program))
  @process := {:state  :terminated
               :result 2
               :vars   {}}

  "composition with variables, no signals"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom 2)) (def b (m/watch !b))
  (def program (dataflow (+ @a (inc @b))))                  ; ??? can you auto add signals? What are the rules
  (def process (debug! program))
  @process := {:state  :running
               :vars   {'a 1                                ; the watches are in the heap
                        'b 2}}                              ; but no incremental maintenance. Slow!
  ; TODO draw viz, it has one node
  ; it fused, like this: ((fn [a b] (+ a (inc b))) @a @b)

  (swap! !b inc)
  @process := {:state  :running
               :vars   {'a 1 'b 2}}

  "composition with variables, with signals"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom 2)) (def b (m/watch !b))
  (def program (dataflow (let [a2 @a                        ; explicit signals
                               b2 @b]
                           (+ a2 (inc b2)))))
  (def process (debug! program))
  @process := {:state :running
               :vars  {'a2 1 'b2 2} #_{'a 1 'b 2}}          ; ??? Does let introduces signals? Deref?

  (swap! !b inc)
  @process := {:state :running
               :vars  {'a2 1 'b2 3} #_{'a 1 'b 2}}

  "composition with no variables"
  (def program (dataflow (+ 1 (inc 2))))                    ; still static
  (def process (debug! program))
  @process := {:state  :terminated
               :result 3
               :vars   {}}
  ; ??? Draw the viz, there are no signals in this static program

  "clojure foreign fn call in response to variability"
  (def !a (atom 1))
  (def a (m/watch !a))
  (def f inc)
  (def program (dataflow (f @a)))
  (def process (debug! program))
  @process := {:state  :running
               #_#_:result 2                                ; ??? Result would be different then 'a, do we want to track it?
               :vars   {'a 1}}

  "incrementally maintain result in response to variable change"
  (swap ! !a inc)
  @process := {:state  :running
               #_#_:result 3                                ; ??? Result would be different then 'a, do we want to track it?
               :vars   {'a 2}}

  "dataflow apply (reactive fn)"
  (def !f (atom +)) (def f (m/watch !f))
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (@f @a 2)))                        ; variable function in fncall position
  (def process (debug! program))
  @process := {:state  :running
               #_#_:result 3
               :vars   {'f +
                        'a 1}}
  (reset! !a 11)
  @process := {:state  :running
               #_#_:result 13
               :vars   {'f +
                        'a 11}}
  (reset! !f -)
  @process := {:state  :running
               #_#_:result 9
               :vars   {'f -
                        'a 11}}

  "simplest diamond, no let"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (+ @a @a)))                        ; ??? how many times does @ appear in the viz?
  (def process (debug! program))
  @process := {:state  :running
               #_#_:result 2
               :vars   {'a 1}}

  ; Dustin opinion:
  ; the ast should match the viz
  ; two @ in ast -> two triangles in viz
  ; There are no test cases above this point about optimizers so we cannot talk about that

  "same thing with let?"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow
                 (let [b @a]
                   (+ b b))))                             ; this is just clojure, 100% foreign call
  ; feels like reagent? (not a bad thing)
  (def process (debug! program))
  @process := {:state  :running
               :vars   {'a 1       ;; ??? Not introduced by let (meaningful?)
                        #_#_'b 1}} ;; ??? Introduced by let     (meaningful?)

  "combined example - incremental maintain clojure computation with variable"
  (def !a (atom 1))
  (def a (m/watch !a))
  (def program (m/cp (println (dataflow (+ @a 2)))))        ; ??? How to bridge RT dataflow exprs to OS effects
  (def process (debug! program))
  @process := {:state       :running
               :result      3
               :vars {'a 1}}

  ; Same heap, same result, different program, same flowchart viz (?)
  ; Why are there two ways to represent the same dataflow structure?


  ; different programs with same value, same result
  ; different heap
  ; doesn't let just inline? How are they different?




  ; Dustin/Geoffrey cutoff point, todo go through this

  ;; GG: Nonsense, seems to indicate let don't introduces signals, `@` does.
  "Let introduces a signal on constrants"
  (def !b (atom 0)) (def b (m/watch !b))
  (def program (dataflow (let [a 1]
                           [a @b])))
  @process := {:state       :running
               :signal-heap {'a 1
                             'b 0}}

  (comment
    "Diamond" ; Waiting for "Let introduces a signal" to be cleared up
    (def !a (atom 0))
    (def a (m/watch !a))
    (def program (dataflow (let [b @a
                                 c (inc b)
                                 d (dec c)]
                             [c d])))
    (def process (debug! program))
    @process := {:state :running
                 :vars  {'a 0
                         'b 0
                         'c 1
                         'd -1}})


  "`if` picks one branch or the other"
  (def !a (atom 0)) (def a (m/watch !a))
  (def !b (atom :b)) (def b (m/watch !a))
  (def !c (atom :c)) (def c (m/watch !a))

  (def program (dataflow (if (odd? a) b c)))
  (def process (debug! program))

  @process := {:state :running
               :vars  {'a 0
                       '% :b}}

  (swap! !a inc)
  @process := {:state :running
               :vars  {'a 1
                       '% :c}}

  (reset! !c :c-updated)
  @process := {:state :running
               :vars  {'a 1
                       '% :c-updated}}

  "`case` dispatch statically to one branch"
  (def !a (atom 0)) (def a (m/watch !a))
  (def !b (atom :b)) (def b (m/watch !b))
  (def !c (atom :c)) (def c (m/watch !c))
  (def !d (atom :d)) (def d (m/watch !d))

  (def program (dataflow (case a
                           0 b
                           1 c
                           d)))
  (def process (debug! program))

  @process := {:state :running
               :vars  {'a 0
                       '% :b}}

  (swap! !a inc)
  @process := {:state :running
               :vars  {'a 1
                       '% :c}}

  (swap! !a inc)
  @process := {:state :running
               :vars  {'a 2
                       '% :d}}

  (reset! !d :d-updated)
  @process := {:state :running
               :vars  {'a 2
                       '% :d-updated}}



  "Destructuring"

  (def !pair [:a 1]) (def pair (m/watch !pair))
  (def program (let [[a b] @pair]
                 [a b]))
  (def process (debug! program))
  @process := {:state :running
               :vars  {'pair [:a 1]
                       '%    [:a 1]}}

  (comment
    ;; A destructuring example, for record
    (def pair [:a 1])

    (macroexpand '(let [[a b] pair]))
    :=
    (let* [vec__10831 pair
           a (clojure.core/nth vec__10831 0 nil)
           b (clojure.core/nth vec__10831 1 nil)])

    (macroexpand-dataflow '(let [[a b] pair]))
    :=
    (let [_1 (m/signal! pair)
          _2 (m/signal! (m/latest #(nth % 0) _1))
          _3 (m/signal! (m/latest #(nth % 1) _1))]))

  ;
  ;:= ((fn [a] (vector (inc a) (dec a))) @a)
  ;
  ;(let [b (inc a)
  ;      c (inc a)
  ;      d (inc a)]
  ;  )
  ;
  ;(fn ^:ifn [a :! Flow]
  ;  (let [a! (m/signal 'a (inc a))
  ;        b! (m/signal 'b (inc b))
  ;        c! (m/signal 'c (inc c))]
  ;    ...))
  ;
  ;;(let [% (inc (inc (inc a)))] ...)
  ;
  ;;'(let [x (fib @a)] (vector x x))
  ;;'(vector (fib @a) (fib @a))
  ;
  ;(def process (debug! program))
  ;@process := {:state :running
  ;             :heap  {'a 0                                 ; inputs appear in the heap
  ;                     'b 0
  ;                     'c _
  ;                     'd _
  ;                     '% [1 -1]}}
  ;; Leo likes this test case, no promises. Keys of the heap should be technical IDs and binding in separate slot
  ;; what's a binding? The names of the let
  ;
  ;(-> @process :heap count) := 5                            ; Leo thinks this is OK

  ; Leo insight triggered by "inputs appear in heap":
  ; Deref in process returns snapshot of variable state ..maybe the bindings with
  ; the original AST could be exposed with dedicated methods of the process type
  ; ...the point is it's not mutable so it doesn't have to be in deref but it should be exposed anyways
  ; ... its useful debug info ... how do I know that a given slot frame matches a specific point in the AST

  )


