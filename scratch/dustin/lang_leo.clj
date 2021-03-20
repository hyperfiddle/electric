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


  (:require [hfdl.lang :refer [dataflow spawn <|]]
            [hfdl.lib :refer [ifn $] ]
            [minitest :refer [tests]]
            [missionary.core :as m]))

(tests

  ; on termination, must produce explicit termination frame?
  ; (The GC operation must be traced)
  ; Or, just free all signals
  ; Same with :state :running – if there are signals not freed, it's running.


  "programs without variability terminate immediately"
  (def program (dataflow 1))
  (def process (debug! program))                            ; (trace!) ?
  @process := {:state  :terminated ; Because the program is entirely static
               :result 1           ; ??? it terminated with 1
               :trace  [{1 1}]
               ; where is the {1 ::free}, or is this program still running?
               :vars   {}}
  ;; Terminated -> nothing to visualize
  ; Dustin March 20 - this is a running program producing constantly 1
  ;   (unless the optimizer flattens the constants)

  ; how does shell user ask the entrypoint for the result of a program that maybe terminated?
  ; need a "shell" - C-z, C-c, print result eventually

  "clojure foreign interop with a value"
  (def a 1)                                                 ; value
  (def program (dataflow a))                                ; clojure interop with a value
  (def process (debug! program))
  @process := {:state  :terminated
               :result 1
               :trace  [{a 1}]
               :vars   {}}
  ;; Terminated -> nothing to visualize

  "clojure foreign interop to a variable"
  "deref adds a signal at the callsite. Not watch"
  ; variable is not overloaded in clojure so we can take that word
  (def !a (atom 1))
  (def a (m/watch !a))                                      ; HFDL variable (missionary flow)
  (def program (dataflow @a))                               ; clojure interop with a HFDL variable
  (def process (debug! program))
  @process := {:state :running ; maintain result forever
               :trace [{'a _ '@a 1}]
               :vars  {'a  _ '@a 1}}                               ; @ introduced a signal

  "maintain result in response to variable changes"
  (swap! !a inc)
  @process := {:state :running
               :trace [{'a _ '@a 1}
                       {'@a 2}]
               :vars  {'a  _
                       '@a 2}}

  "Transition between running and terminated state"
  (def !a (atom 0))
  (def a (->> (m/watch !a)
              (m/transform (take-while even?))))

  (def program (dataflow @a))
  (def process (debug! program))

  @process := {:state :running
               :trace [{'a _ '@a 0}]
               :vars  {'a  _ '@a 0}}

  (reset! !a 2)
  @process := {:state :running
               :trace [{'a _ '@a 0}
                       {'@a 2}]
               :vars  {'a  _ '@a 2}}

  (reset! !a 1)
  @process := {:state  :terminated ; terminate process because the underlying discrete flow terminated
               :result 1           ; termination GCs the vars and produces a stack value
               :trace [{'a _ '@a 0}
                       {'@a 2}
                       {'@a ::hfdl/free}]
               :vars   {}}

  "clojure foreign fn call"
  (def f inc)
  (def program (dataflow (f 1)))
  (def process (debug! program))
  @process := {:state  :terminated ; terminate because there are no alive signals
               :result 2
               :trace  [{1 1
                         'f f
                         '(f 1) 2}]
               :vars   {}}

  "clojure foreign fn call, composed"
  (def f inc)
  (def program (dataflow (f (f 1))))                        ; still no alive signals
  (def process (debug! program))
  @process := {:state  :terminated
               :result 3
               :trace  [{1 1
                         'f f
                         '(f 1) 2
                         '(f (f 1)) 3}]
               :vars   {}}

  "clojure foreign fn call in response to variability"
  (def !a (atom 1)) (def a (m/watch !a))
  (def f inc)
  (def program (dataflow (f @a)))
  (def process (debug! program))
  @process := {:state :running
               :trace [{'a      _
                        '@a     1
                        'f      f
                        '(f @a) 2}]
               :vars  {'a  _
                       '@a 1
                       'f  f
                       #_#_'(f @a) 2}}                      ; only in the trace! signal is a different color

  "dataflow apply (reactive fn)"
  (def !f (atom +)) (def f (m/watch !f))
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (@f @a 2)))                        ; variable function in fncall position
  (def process (debug! program))
  (reset! !f -)
  @process := {:state :running
               :trace [{'f         _
                        '@f        clojure.core/+
                        'a         _
                        '@a        1
                        2          2
                        '(@f @a 2) 3}
                       {'@f        clojure.core/-
                        '(@f @a 2) -1}]}

  "clojure ASTs do not introduce intermediate signals"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom 2)) (def b (m/watch !b))
  (def program (dataflow (+ @a (inc @b))))                  ; does '(inc @b) get a signal?
  ; Dag is something like this: ((fn [a b] (+ a (inc b))) @a @b)
  (def process (debug! program))
  (swap! !b inc)
  @process := {:state :running
               :trace [{'a               _
                        '@a              1
                        'b               _
                        '@b              2
                        'inc             clojure.core/inc
                        '+               clojure.core/+
                        '(inc @b)        3
                        '(+ @a (inc @b)) 4}
                       {'@b 3
                        '(inc @b)        4
                        '(+ @a (inc @b)) 5}]}

  "does let introduce signals"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom 2)) (def b (m/watch !b))
  (def program (dataflow (let [a2 @a
                               b2 @b]
                           (+ a2 (inc b2)))))
  (def process (debug! program))
  (swap! !b inc)
  @process := {:state :running
               :trace [{'a               _
                        '@a              1
                        'b               _
                        '@b              2
                        'inc             clojure.core/inc
                        '+               clojure.core/+
                        '(inc @b)        3
                        '(+ @a (inc @b)) 4}
                       ; let does not allocate
                       {'@b 3
                        '(inc @b)        4
                        '(+ @a (inc @b)) 5}]}

  "simplest diamond, no let"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (+ @a @a)))                        ; two @
  (def process (debug! program))
  @process := {:state :running
               :trace [{'a         _
                        '@a        1
                        '+         clojure.core/+
                        '(+ @a @a) 2}]}                     ; one signal! reused twice

  ; This test implies the AST (two @) does not match the Viz (one @)
  ; this happens without an optimizer?
  ; what is an optimizer?

  "same thing with let?"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow
                (let [b @a]
                   (+ b b))))                             ; this is just clojure, 100% foreign call
  (def process (debug! program))
  @process := {:state :running
               :trace [{'a         _
                        '@a        1
                        '+         clojure.core/+
                        '(+ @a @a) 2}]}                     ; let does not introduce signal

  ; Same heap, same result, different program... same flowchart viz!
  ; Why are there two ways to represent the same dataflow structure?
  ; Because leo-lang priorities clojure interop over visualization, viz is a non-goal

  ; different programs with same value, same result
  ; different heap
  ; doesn't let just inline? How are they different?




  ; Dustin/Geoffrey cutoff point, todo go through this

  ;; GG: Nonsense, seems to indicate let don't introduces signals, `@` does.
  "Let introduces a signal on constants"
  (def !b (atom 0)) (def b (m/watch !b))
  (def program (dataflow (let [a 1]
                           [a @b])))
  @process := {:state :running
               :trace [{1       1
                        'b      _
                        '@b     0
                        '[1 @b] [1 0]}]}

  (comment
    "Naming slots by their AST symbol is ambiguous"
    (def !a (atom 0)) (def a (m/watch !b))
    (def program (dataflow
                  @a
                  [(let [a 1] a)
                   (let [a 2] a)]))
    (def process (debug! program))
    @process := {:state :running
                 :syms  '{a #{1 2}}
                 :expr  '[1 2]
                 :vars  {'a    _
                         '@a   0
                         1     1
                         2     2
                         [1 2] [1 2]}})

  "Diamond"
  (def !a (atom 0))
  (def a (m/watch !a))
  (def program (dataflow (let [b @a
                               c (inc b)
                               d (dec b)]
                           [c d])))
  (def process (debug! program))
  @process := {:state :running
               :trace [{'a                   _
                        '@a                  0
                        '(inc @a)            1
                        '(dec @a)            -1
                        '[(inc @a) (dec @a)] [1 -1]}]}


  "`if` picks one branch or the other"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom :b)) (def b (m/watch !a))
  (def !c (atom :c)) (def c (m/watch !a))

  (def program (dataflow @(if (odd? @a) b c)))
  ;(def program (dataflow (if (odd? @a) @b @c)))
  (def process (debug! program))

  (swap! !a inc)
  (reset! !c :c-updated)
  @process := {:state :running
               :trace [{'a                  _
                        '@a                 1
                        'odd?               clojure.core/odd?
                        'b                  _
                        'c                  _
                        '(odd? @a)          true
                        '(if (odd? a) b c)  _
                        '@(if (odd? a) b c) :b}
                       {'@a                 2
                        '(odd? @a)          false
                        '@(if (odd? a) b c) :c}
                       {'@(if (odd? a) b c) :c-updated}]}

  "`case` dispatch statically to one branch"
  (def !a (atom 0)) (def a (m/watch !a))
  (def !b (atom :b)) (def b (m/watch !b))
  (def !c (atom :c)) (def c (m/watch !c))
  (def !d (atom :d)) (def d (m/watch !d))

  (def program (dataflow @(case @a
                            0 b
                            1 c
                            d)))
  (def process (debug! program))

  @process := {:state :running
               :vars  {'a                    _
                       '@a                   0
                       0                     0
                       1                     1
                       'b                    _
                       'c                    _
                       'd                    _
                       '(case @a 0 b 1 c d)  _
                       '@(case @a 0 b 1 c d) :b}}

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
  (def program (dataflow (let [[a b] @pair]
                           [a b])))
  (def process (debug! program))
  @process := {:state :running
               :vars  {'pair                                  _
                       '@pair                                 [:a 1]
                       '(nth @pair 0 nil)                     :a
                       '(nth @pair 1 nil)                     1
                       '[(nth @pair 0 nil) (nth @pair 1 nil)] [:a 1]}}

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


  "does dataflow compose with missionary"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (+ @a 2)))        ; not well-typed
  (def process (debug! program))
  @process := {:state :running
               :vars  {'a        _
                       '@a       1
                       '+        +
                       2         2
                       '(+ @a 2) 3}}

  (m/cp (println process))                 ; compose processes?
  ;; Defered until we talk about spawn

  "Spawn is to dataflow what deref is to flow."

  (def !a (atom 1)) (def a (m/watch !a))
  (def program1 (dataflow @a))
  (def program2 (dataflow (spawn program1)))
  (def process (debug! program2))
  @process := {:state :running
               :vars  {'program1         {'a _, '@a 1}
                       '(spawn program1) 1}}

  "Spawning twice is allowed by RT"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program1 (dataflow @a))
  (def program2 (dataflow [(spawn program1) (spawn program1)]))
  (def process (debug! program2))
  @process := {:state :running
               :vars  {'program1                            {'a _, '@a 1}
                       '(spawn program1)                    1
                       '[(spawn program1) (spawn program1)] [1 1]}}

  ;; (ifn [x] (inc x)) ;; => (fn [>x] (dataflow (let [x @>x] (inc x))))
  ;; (ifn [x] (inc x)) ;; => (fn [>x] (dataflow (inc @x)))

  ;; (dataflow ((fn [>x] (inc @>x)) (extend 1)))

  ;; (declare plus-one)
  ;; ($ plus-one 1)    ;; => (spawn (plus-one (extend 1)))


  ;; |> extend
  ;; <| deref

  (def program (dataflow (spawn (inc 1))))
  (def process (debug! program))
  @process := {:state :running
               :vars  {'(inc 1)         {1        1
                                         'inc     inc
                                         '(inc 1) 2}
                       '(spawn (inc 1)) 2}}

  "Ifn"
  (def plus-one (ifn [x] (inc x)))
  (def program (dataflow ($ plus-one 1)))
  (def process (debug! program))
  @process := {:state :running
               :vars  {1                              1
                       '(extend 1)                    _
                       'plus-one                      plus-one
                       '(plus-one (extend 1))         {'x        _
                                                       '@x       1
                                                       '(inc @x) 2}
                       '(spawn (plus-one (extend 1))) 2}}

  "Composed ifn calls"
  (def plus-one (ifn [x] (inc x)))
  (def minus-one (ifn [x] (dec x)))
  (def program (dataflow ($ minus-one ($ plus-one 1))))
  (def process (debug! program))
  @process := {:state :running
               :vars  {1                                                  1
                       '(extend 1)                                        _
                       'plus-one                                          plus-one
                       'minus-one                                         minus-one
                       '(plus-one (extend 1))                             {'x        _
                                                                           '@x       1
                                                                           '(inc @x) 2}
                       '(spawn (plus-one (extend 1)))                     2
                       '(minus-one (spawn (plus-one (extend 1))))         {'x        _
                                                                           '@x       2
                                                                           '(dec @x) 1}
                       '(spawn (minus-one (spawn (plus-one (extend 1))))) 1}}

  "Nested ifn calls"
  (def inner (ifn [x] (inc x)))
  (def outer (ifn [x] ($ inner x)))
  (def program (dataflow ($ outer 1)))
  (def process (debug! program))
  @process := {:state :running
               :vars  {1                           1
                       '(extend 1)                 _
                       'inner                      inner
                       'outer                      outer
                       '(outer (extend 1))         {'x                          _
                                                    '@x                         1
                                                    '(inner (extend @x))        {'x        _
                                                                                 '@x       1
                                                                                 '(inc @x) 2}
                                                    '(spawn (inner (extend 1))) 2}
                       '(spawn (outer (extend 1))) 2}}

  ; What are the requirements of debug?

  ;; TODO: Preserve last heap dump when terminating. GC the heap, just keep the last snapshot.
  "Don't use spawn"
  (def plus-one (ifn [x] (inc x)) )
  (def program (dataflow (plus-one (extend 1))))
  (def process (debug! program))
  @process := {:state :running
               :vars  {1                      1
                       '(extend 1)            _
                       'plus-one              plus-one
                       '(plus-one (extend 1)) '[[:local _ #_#function[dustin.lang-leo/eval26418/fn--26419/fn--26420]]
                                                [:global hfdl.lang/<|]
                                                [:local _ #_#function[hfdl.impl.runtime/pure/fn--16995]]
                                                [:apply 1 [2]]
                                                [:apply 0 [3]]]}}

  (comment  ;; Trash
    "Manual join/deref"
    (def join <|)
    (def program (dataflow (join (plus-one (extend 1)))))
    (def process (debug! program))
    @process := {:state :running
                 :vars  {1                             1
                         '(extend 1)                   _
                         'plus-one                     plus-one
                         '(plus-one (extend 1))        _ ; opaque flow
                         '(join (plus-one (extend 1))) 2
                         }})

  "Join extend is identity"
  (def program (dataflow (join (extend 1))))
  (def process (debug! program))
  @process := {:state :running
               :vars  {1                  1
                       '(extend 1)        _
                       '(join (extend 1)) 1}}

  "extend after join is identity"
  ;; This test can't pass without an optimization phase canceling out join after
  ;; extend or extend after join.

  (def program (dataflow (extend (join (extend 1)))))
  (def process (debug! program))
  @process := {:state :running
               :vars  {1                           1
                       '(extend 1)                 ?a
                       '(join (extend 1))          1
                       '(extend (join (extend 1))) ?a}}

  ;; The purpose of debug is to get a technically accurate description of the
  ;; state of the system. It could be used as an input to a viz or for test
  ;; cases. Test cases would catch a change in the memory representation. It's
  ;; better for it to be accurate then pretty.
  ;; L: Keys could be integers + source maps.
  ;; D: Symbols are accurate, it's fine to use them. Integers makes
  ;;    it hard to understand the test case.


  extend-seq :: (a -> k) -> Flow [a] -> Flow [a]
  reactive-for :: (Flow a -> Dataflow b) -> (a -> k) -> Flow [a] -> Dataflow [b]

  (macroexpand-1 '(rfor [x identity @xs] (inc x)))
  := (dataflow (sequence (map (fn [>x] (spawn (dataflow (inc @>x))) (extend-seq identity >xs)))))

  ;; Heap

  (comment ;; WIP, don't rely on it.
    "Reactive for"
    (def !a (atom [1 2 3])) (def a (m/watch !a))
    (def program (dataflow (rfor [x identity @a] (inc x))))
    (def process (debug! program))
    @process := {:state :running
                 :vars  {'a                    _
                         'identity             identity
                         '(mystère identity a) [_ _ _]
                         '(fn [>x] (dataflow (inc @>x)))}}
    )


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

  ; what is effect fusion
  ; JIT detects two signals can be one
  )
