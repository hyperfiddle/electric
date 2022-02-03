(ns dustin.lang-leo
  "tests to communicate about leo lang"
  (:require [hfdl.lang :refer [dataflow debug! heap-dump] :as hfdl]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]
            [hfdl.viz :refer [show! dot differential-dot]]
            [clojure.data :refer [diff]]))

(tests
 "programs without variability terminate immediately"
 (def program (dataflow 1))
 (def process (debug! program))
 @process := {:status :terminated
              :log    [{1 1}]}
 ;; (show! (dot program))
 )

(tests
 "clojure foreign interop with a value"
 (def a 1)                                                 ; value
 (def program (dataflow a))                                ; clojure interop with a value
 (def process (debug! program))
 @process := {:status :terminated
              :log    [{`a 1}]}
 ;; (show! (dot program (heap-dump @process)))
 )

(tests
 "clojure foreign interop to a variable"
 "deref adds a signal at the callsite. Not watch"
 ;; variable is not overloaded in clojure so we can take that word
 (def !a (atom 1))
 (def a (m/watch !a))                                      ; HFDL variable (missionary flow)
 (def program (dataflow @a))                               ; clojure interop with a HFDL variable
 (def process (debug! program))
 @process := {:status :running ; maintain result forever
              :log    [{`a a, `@a 1}]}                     ; @ introduced a signal
 ;; (show! (dot program (heap-dump @process)))

 "maintain result in response to variable changes"
 (swap! !a inc)
 @process := {:status :running
              :log    [{`a a, `@a 1}
                       {`@a 2}]}
 ;; (show! (dot program (heap-dump @process)))
 )

(tests
 "Transition between running and terminated state"
 (def !a (atom 0))
 (def a (->> (m/watch !a)
             (m/eduction (take-while even?))))

 (def program (dataflow @a))
 (def process (debug! program))

 @process := {:status :running
              :log    [{`a a, `@a 0}]}

 (reset! !a 2)
 @process := {:status :running
              :log    [{`a a, `@a 0}
                       {`@a 2}]}

 (reset! !a 1)
 @process := {:status :terminated ; terminate process because the underlying discrete flow terminated
              :log    [{`a a, `@a 0}
                       {`@a 2}
                       {`@a ::hfdl/free}]}

 ;; (show! (dot program (heap-dump @process)))
 )

(tests
 "clojure foreign fn call"
 (def program (dataflow (inc 1)))
 (def process (debug! program))
 @process := {:status :terminated ; terminate because there are no alive signals
              :log    [{`inc     inc
                        1        1
                        `(inc 1) 2}]}

 ;; (show! (dot program))
 ;; (show! (dot program (heap-dump @process)))
 )

(tests
 "clojure foreign fn call, composed"
 (def program (dataflow (inc (inc 1)))) ; still no alive signals
 (def process (debug! program))
 @process := {:status :terminated
              :log    [{1              1
                        `inc           inc
                        `(inc 1)       2
                        `(inc (inc 1)) 3}]}

 ;; (show! (dot program (heap-dump @process)))
 )

(tests
 "clojure foreign fn call in response to variability"
 (def !a (atom 1)) (def a (m/watch !a))
 (def program (dataflow (inc @a)))
 (def process (debug! program))
 @process := {:status :running
              :log    [{`a      a
                        `inc      inc
                        `@a     1
                        `(inc @a) 2}]}

 ;; (show! (dot program (heap-dump @process)))
 )

(tests
 "dataflow apply (reactive fn)"
 (def !f (atom +)) (def f (m/watch !f))
 (def !a (atom 1)) (def a (m/watch !a))
 (def program (dataflow (@f @a 2)))                        ; variable function in fncall position
 (def process (debug! program))
 (def render (differential-dot program))
 @process := {:status :running,
              :log    [{`f         f
                        `@f        +
                        2          2
                        `a         a
                        `@a        1,
                        `(@f @a 2) 3}]}

 ;; (show! (render (heap-dump @process)))

 (reset! !f -)
 @process := {:status :running
              :log    [{`f         f
                        `@f        +
                        2          2
                        `a         a
                        `@a        1,
                        `(@f @a 2) 3}
                       {`@f        -
                        `(@f @a 2) -1}]})

;; (show! (render (heap-dump @process)))

(tests

 "Clojure ASTs do introduce intermediate signals"
 ;; but this may be optimized (by a JIT or a compiler)
 ;; DAG could be something like `((fn [a b] (+ a (inc b))) @a @b)`
 (def !a (atom 1)) (def a (m/watch !a))
 (def !b (atom 2)) (def b (m/watch !b))
 (def program (dataflow (+ @a (inc @b))))
 (def process (debug! program))
 (def render (differential-dot program))
 ;; (show! (render (heap-dump @process)))
 (swap! !b inc)
 ;; (show! (render (heap-dump @process)))
 @process := {:status :running
              :log    [{`b               b,
                        `@b              2,
                        `inc             inc,
                        `(inc @b)        3,
                        `+               +,
                        `a               a,
                        `@a              1,
                        `(+ @a (inc @b)) 4}
                       {`@b              3,
                        `(inc @b)        4,
                        `(+ @a (inc @b)) 5}]})

(tests
 "does let introduce signals"
 (def !a (atom 1)) (def a (m/watch !a))
 (def !b (atom 2)) (def b (m/watch !b))
 (def program-with-let (dataflow (let [a2 @a
                                       b2 @b]
                                   (+ a2 (inc b2)))))
 ;; this program = program above
 program := program-with-let
 ;; this process log = process log above
 (def process-with-let (debug! program))
 (swap! !b inc)
 @process := @process-with-let
 (def render (differential-dot program))
 ;; (show! (render (heap-dump @process)))
 ;; (show! (render (heap-dump @process)))
 @process-with-let := {:status :running
                       :log    [{`b               b,
                                 `@b              2,
                                 `inc             inc,
                                 `(inc @b)        3,
                                 `+               +,
                                 `a               a,
                                 `@a              1,
                                 `(+ @a (inc @b)) 4}
                                {`@b              3,
                                 `(inc @b)        4,
                                 `(+ @a (inc @b)) 5}]})

(tests

 "simplest diamond, no let"
 (def !a (atom 1)) (def a (m/watch !a))
 (def program (dataflow (+ (inc @a) (dec @a))))                        ; two @
 (def process (debug! program))
 @process ;; => {:status :running, :log []}
 (show! (dot program (heap-dump @process)))
 @process := {:status :running
              :log    [{`a                     a
                        `@a                    1
                        `dec                   dec
                        `(dec @a)              0
                        `inc                   inc
                        `(inc @a)              2
                        `+                     +
                        `(+ (inc @a) (dec @a)) 2}]})                     ; one signal! reused twice

(tests
 "same thing with let?"
 (def !a (atom 1)) (def a (m/watch !a))
 (def program (dataflow (let [b @a]
                          (+ b b))))      ; this is just clojure, 100% foreign call
 (def process (debug! program))
 @process := {:status :running
              :log    [{`a         a
                        `@a        1
                        `+         +
                        `(+ @a @a) 2}]})                     ; let does not introduce signal

;; Same heap, same result, different program... same flowchart viz!
;; Why are there two ways to represent the same dataflow structure?
;; Because leo-lang priorities clojure interop over visualization, viz is a non-goal

;; different programs with same value, same result
;; different heap
;; doesn't let just inline? How are they different?

(tests
 "Diamond"
 (def !a (atom 0))
 (def a (m/watch !a))
 (def program (dataflow (let [b @a
                              c (inc b)
                              d (dec b)]
                          (vector c d))))
 (def process (debug! program))
 @process := {:status :running
              :log    [{`vector                     vector
                        `inc                        inc
                        `dec                        dec
                        `a                          a
                        `@a                         0
                        `(dec @a)                   -1
                        `(inc @a)                   1
                        `(vector (inc @a) (dec @a)) [1 -1]}]})

(tests

 "`if` picks one branch or the other"
 (def !a (atom 1)) (def a (m/watch !a))
 (def !b (atom :b)) (def b (m/watch !b))
 (def !c (atom :c)) (def c (m/watch !c))

 (def program (dataflow @(if (odd? @a) b c)))
 (def process (debug! program))

 (def render (differential-dot program))

 ;; (show! (render (heap-dump @process)))

 @process
 :=
 {:status :running
  :log    [{nil                                                                         nil,
            `@a                                                                         1,
            `get                                                                        get,
            `(get (hash-map nil (unquote c) false (unquote c)) (odd? @a) (unquote b))   _,
            `c                                                                          c,
            `@(get (hash-map nil (unquote c) false (unquote c)) (odd? @a) (unquote b))  b,
            `b                                                                          b,
            `(unquote b)                                                                _,
            `odd?                                                                       odd?,
            `@@(get (hash-map nil (unquote c) false (unquote c)) (odd? @a) (unquote b)) :b,
            false                                                                       false,
            `hash-map                                                                   hash-map,
            `a                                                                          a,
            `(odd? @a)                                                                  true,
            `(hash-map nil (unquote c) false (unquote c))                               {nil   _,
                                                                                         false _},
            `(unquote c)                                                                _}]}

 (swap! !a inc)
 @process
 :=
 {:status :running
  :log    [_
           {`@a                                                    2
            `(odd? @a)                                             false
            `(get (hash-map nil (unquote c) false (unquote c)) (odd? @a) (unquote b))   _
            `@(get (hash-map nil (unquote c) false (unquote c)) (odd? @a) (unquote b))  c
            `@@(get (hash-map nil (unquote c) false (unquote c)) (odd? @a) (unquote b)) :c}]}

 ;; (show! (render (heap-dump @process)))

 (reset! !c :c-updated2)

 @process
 :=
 {:status :running
  :log    [_
           _
           {`@@(get (hash-map nil (unquote c) false (unquote c)) (odd? @a) (unquote b)) :c-updated2}]})

;; (show! (render (heap-dump @process)))

;; @process := {:status :running
;;              :log [{'a                  _
;;                       '@a                 1
;;                       'odd?               clojure.core/odd?
;;                       'b                  _
;;                       'c                  _
;;                       '(odd? @a)          true
;;                       '(if (odd? a) b c)  _
;;                       '@(if (odd? a) b c) :b}
;;                      {'@a                 2
;;                       '(odd? @a)          false
;;                       '@(if (odd? a) b c) :c}
;;                      {'@(if (odd? a) b c) :c-updated}]}

(tests
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

 @process := {:status :running
              :log    [{`a                    _
                        `@a                   0
                        0                     0
                        1                     1
                        `b                    _
                        `c                    _
                        `d                    _
                        `(case @a 0 b 1 c d)  _
                        `@(case @a 0 b 1 c d) :b}]}

 (swap! !a inc)
 @process := {:status :running
              :log    [_
                       {`@a                   1
                        `@(case @a 0 b 1 c d) :c}]}

 (swap! !a inc)
 @process := {:status :running
              :log    [_
                       _
                       {`@a                   2
                        `@(case @a 0 b 1 c d) :d}]}

 (reset! !d :d-updated)
 @process := {:status :running
              :log    [_
                       _
                       _
                       {`@(case @a 0 b 1 c d) :d-updated}]})

(tests
 "Destructuring"

 (def !pair (atom [:a 1])) (def pair (m/watch !pair))
 (def program (dataflow (let [[a b] @pair]
                          (vector a b))))
 (def process (debug! program))
 @process := {:status :running
              :log [{nil                                           nil
                     0                                             0
                     1                                             1
                     `vector                                       vector
                     `nth                                          nth
                     `pair                                         pair
                     `@pair                                        [:a 1]
                     `(nth @pair 0 nil)                            :a
                     `(nth @pair 1 nil)                            1
                     `(vector (nth @pair 0 nil) (nth @pair 1 nil)) [:a 1]}]})

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

(tests
 "does dataflow compose with missionary"  ; yes
 (def !a (atom 1)) (def a (m/watch !a))
 (def program (dataflow (+ @a 2)))        ; well-typed
 (def process (debug! program))
 @process := {:status :running
              :log [{`a        a
                     `@a       1
                     `+        +
                     2         2
                     `(+ @a 2) 3}]})

;; (m/cp (println (m/? program)))                 ; compose processes?
;; TODO Defered until we talk about spawn

(tests
 "dataflow composes like flow"
 (def !a (atom 1)) (def a (m/watch !a))

 (defn program1 [a b] (dataflow (+ @a b)))
 (def program2 (dataflow [@(program1 a 1) @(program1 a 2)]))

 ;; main goal of compiler phase in the IL is to free the interpreter from macroexpansion
 ;; which requires a full clojure evaluator. We don't want to require the client to have
 ;; full eval capabilities (too expensive).

 (def process (debug! program2))
 ;; (show! (dot program2 (heap-dump @process)))
 @process := {:status :running
              :log    [{1                                         1
                        2                                         2
                        `a                                        a
                        `program1                                 program1
                        `vector                                   vector
                        `(program1 a 1)                           {0 {1          1
                                                                      a          a
                                                                      `@~a       1
                                                                      `+         +
                                                                      `(+ @~a 1) 2}}
                        `(program1 a 2)                           {0 {2          2
                                                                      a          a
                                                                      `@~a       1
                                                                      `+         +
                                                                      `(+ @~a 2) 3
                                                                      }}
                        `@(program1 a 1)                          2
                        `@(program1 a 2)                          3
                        `(vector @(program1 a 1) @(program1 a 2)) [2 3]}]})

(tests
 "Like flows, dataflows are RT and run once when linked twice"
 (def !a (atom 1)) (def a (m/watch !a))
 (def program1 (dataflow @a))
 (def program2 (dataflow [@program1 @program1]))
 (def process (debug! program2))
 @process := {:status :running
              :log    [{`vector                       vector
                        `program1                     {0 {`a a, `@a 1}}
                        `@program1                    1
                        `(vector @program1 @program1) [1 1]}]})

(tests
 "X"
 (def program (dataflow @(dataflow (str 1))))
 (def process (debug! program))
 ;;(select-keys (:log @process) []) :=
 @process := {:status :running
              :log    [{`(dataflow (str 1))  {1        1
                                              `str     str
                                              `(str 1) "1"}
                        `@(dataflow (str 1)) "1"}]})

(tests
 "Ifn"
 (def plus-one (fn [x] (dataflow (inc @x))))
 (def program (dataflow @(plus-one ~1)))
 (def process (debug! program))
 @process := {:status :running
              :log    [{1                        1
                        `(unquote 1)             _
                        `plus-one                plus-one
                        ;; TODO minitest missing unification
                        #_#_`(plus-one (unquote 1))  {`inc                  inc
                                                  ?x                    ?x
                                                  `@(unquote ~?x)       1
                                                  `(inc @(unquote ~?x)) 2}
                        `@(plus-one (unquote 1)) 2
                        }]})

(tests
 "Composed ifn calls"
 (def plus-one (fn [x] (dataflow (inc @x))))
 (def minus-one (fn [x] (dataflow (dec @x))))
 (def program (dataflow @(minus-one (plus-one ~1))))
 (def process (debug! program))
 @process := {:status :running
              :log    [{1                           1
                        ~1                          _
                        `plus-one                   plus-one
                        `(plus-one ~1)              {`inc  inc
                                                     'x    _
                                                     `@~'x 2}
                        `minus-one                  minus-one
                        `(minus-one (plus-one ~1))  {`dec        dec
                                                     'x          {`inc  inc
                                                                  'x    _
                                                                  `@~'x 2}
                                                     `@~'x       2
                                                     `(dec @~'x) 1}
                        '@(minus-one (plus-one ~1)) 1}]})

(tests
 "deref after unquote is identity"
 (def program (dataflow @~1))
 (def process (debug! program))
 @process := {:status :terminated
              :log    [{1             1
                        `(unquote 1)  _
                        `@(unquote 1) 1}]})

(tests
 "unquote after deref is identity"
 ;; This test can't pass without an optimization phase canceling out join after
 ;; extend or extend after join.

 (def program (dataflow ~(deref ~1)))                      ; careful ~@ is unquote-splice
 (def process (debug! program))
 @process := {:status :running
              :log    [{1                              1
                        `(unquote 1)                   _ ; ?a
                        `@(unquote 1)                  1
                        `(unquote (deref (unquote 1))) _ ; ?a
                        }]})
