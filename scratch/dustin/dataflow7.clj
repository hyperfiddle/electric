(ns dustin.dataflow7
  "tests to communicate about leo lang"
  (:require [hfdl :refer [dataflow]]
            [minitest :refer [tests]]
            [missionary.core :as m]))

(test

  "programs without variability terminate immediately"
  (def program (dataflow 1))
  (def process (debug! program))                            ; (trace!) ?
  @process := {:state       :terminated                     ; Because the program is entirely static
               :result      1                               ; ??? it terminated with 1
               :signal-heap {'% 1}}                         ; ??? point of dataflow is to cause effects
  ; why save % in the heap? Don't need it, only need intermediate components
  ; this program has no signals
  ;
  ; the process is terminated, why is there a heap? It should be GC
  ; A terminated process should not retain dataflow heap
  ; We're tracing, this is a traced heap i guess

  "run! will GC the heap when process is over"
  (def program (dataflow 1))
  (def process (run! program))
  @process := {:state  :terminated                          ; Because the program is entirely static
               :result 1}                                   ; the heap was GC'ed since we ran with (run!)

  ; how does shell user ask the entrypoint for the result of a program that maybe terminated?
  ; need a "shell" - C-z, C-c, print result eventually

  "clojure foreign interop with a value"
  (def a 1)                                                 ; value
  (def program (dataflow a))                                ; clojure interop with a value
  (def process (debug! program))
  @process := {:state       :terminated
               :result      1
               :signal-heap {'% 1}}

  "clojure foreign interop to a variable"
  ; variable is not overloaded in clojure so we can take that word
  (def !a (atom 1))
  (def a (m/watch !a))                                      ; HFDL variable (missionary flow)
  (def program (dataflow @a))                               ; clojure interop with a HFDL variable
  (def process (debug! program))
  @process := {:state       :running                        ; maintain result forever
               :signal-heap {'a 1                           ; ???
                             '% 1}}                         ; ??? point of dataflow is to cause effects

  "maintain result in response to variable changes"
  (swap ! !a inc)
  @process := {:state       :running
               :signal-heap {'% 2}}

  "clojure foreign fn call"
  (def f inc)
  (def program (dataflow (f 1)))
  (def process (debug! program))
  @process := {:state       :terminated
               :result      2
               :signal-heap {'% 2}}

  "composition with variables, no signals"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom 2)) (def b (m/watch !b))
  (def program (dataflow (+ @a (inc @b))))                  ; can you auto add signals? What are the rules
  (def process (debug! program))
  @process := {:state       :running
               :result      3
               :signal-heap {'a 1                           ; the watches are in the heap
                             'b 2}}                         ; but no incremental maintenance. Slow!
  ; TODO draw viz, it has one node
  ; it fused, like this: ((fn [a b] (+ a (inc b))) @a @b)

  (swap! !b inc)
  @process := {:state       :running
               :result      4
               :signal-heap {'a 1 'b 2}}

  "composition with variables, with signals"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom 2)) (def b (m/watch !b))
  (def program (dataflow (let [a2 @a                        ; explicit signals
                               b2 @b]
                           (+ a2 (inc b2)))))
  (def process (debug! program))
  @process := {:state       :running
               :result      3
               :signal-heap {'a2 1 'b2 2}}                  ; wrong heap. This is fused
  (swap! !b inc)
  @process := {:state       :running
               :result      4
               :signal-heap {'a 1 'b 2}}

  "composition with no variables"
  (def program (dataflow (+ 1 (inc 2))))                    ; still static
  (def process (debug! program))
  @process := {:state       :terminated
               :result      4
               :signal-heap {}}
  ; ??? Draw the viz, there are no signals in this static program

  "clojure foreign fn call in response to variability"
  (def !a (atom 1))
  (def a (m/watch !a))
  (def f inc)
  (def program (dataflow (f @a)))
  (def process (debug! program))
  @process := {:state       :running
               :signal-heap {'a 1
                             '% 2}}

  "incrementally maintain result in response to variable change"
  (swap ! !a inc)
  @process := {:state       :running
               :signal-heap {'a 2
                             '% 3}}

  "combined example - incremental maintain clojure computation with variable"
  (def !a (atom 1))
  (def a (m/watch !a))
  ;(def + +)
  (def program (m/cp (println (dataflow (+ @a 2)))))        ; ??? How to bridge RT dataflow exprs to OS effects
  (def process (debug! program))
  @process := {:state       :running
               :signal-heap {'a 1 '% 3}}

  "dataflow apply (reactive fn)"
  (def !f (atom +)) (def f (m/watch !f))
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (@f @a 2)))                        ; variable function in fncall position
  (def process (debug! program))
  @process := {:state :running :signal-heap {'f +
                                             'a 1
                                             '% 3}}
  (reset! !a 11)
  @process := {:state :running :signal-heap {'f + 'a 11 '% 13}}
  (reset! !f -)
  @process := {:state :running :signal-heap {'f - 'a 11 '% 9}}

  "simplest diamond, no let"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (+ @a @a)))                        ; how many times does @ appear in the viz?
  (def process (debug! program))
  @process := {:state :running :signal-heap {'a 1           ; ???
                                             '% 2}}

  ; Dustin opinion:
  ; the ast should match the viz
  ; two @ in ast -> two triangles in viz
  ; There are no test cases above this point about optimizers so we cannot talk about that

  "same thing with let?"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow
                 (let [a' @a]
                   (+ a' a'))))                             ; this is just clojure, 100% foreign call
  ; feels like reagent? (not a bad thing)
  (def process (debug! program))
  @process := {:state :running :signal-heap {'a' 1 '% 2}}
  ; Same heap, same result, different program, same flowchart viz (?)
  ; Why are there two ways to represent the same dataflow structure?


  ; different programs with same value, same result
  ; different heap
  ; doesn't let just inline? How are they different?




  ; Dustin/Geoffrey cutoff point, todo go through this

  ;(def !a (atom 0))
  ;(def a (m/watch !a))
  ;(def program (dataflow (let [b @a] b)))
  ;; did deref allocate heap or let
  ;@process := {:state :running :signal-heap {'a 1 'b 1 '% 1}}
  ;
  ;"what is let with a literal"
  ;(def program (dataflow (let [a 1] a)))                    ; introduce signal
  ;@process := {:state :terminated :signal-heap {'a 1 '% 1}}
  ;
  ;(def program (dataflow (let [a 1 b 2]
  ;                         )))
  ;
  ;"diamond"
  ;(def !a (atom 0))
  ;(def a (m/watch !a))
  ;
  ;
  ;; What is let? let is a way for the programmer to name a expression component
  ;(def program (dataflow (let [b @a
  ;                             c (inc b)
  ;                             d (dec c)]
  ;                         (vector c d))))
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

(comment

  "Destructuring"
  (def pair [:a 1])

  (macroexpand '(let [[a b] pair]))

  (let* [vec__10831 pair
         a (clojure.core/nth vec__10831 0 nil)
         b (clojure.core/nth vec__10831 1 nil)]
    )

  (let [_1 (m/signal! pair)
        _2 (m/signal! (m/latest #(nth % 0) _1))
        _3 (m/signal! (m/latest #(nth % 1) _1))]
    )

  )
