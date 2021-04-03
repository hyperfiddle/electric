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


  (:require [hfdl.lang :refer [dataflow debug! heap-dump]]
            [hfdl.lib :refer [ifn $] ]
            [minitest :refer [tests]]
            [missionary.core :as m]
            [hfdl.sourcemap :refer [humanize]]
            [hfdl.viz :refer [show! show!* dot differential-dot]]))

(tests

  ;; TODO indicate ::free in the log

  "programs without variability terminate immediately"
  (def program (dataflow 1))
  (def process (debug! program))
  (humanize program (heap-dump @process)) := {1 1}
  @process := {:status :terminated
               :log    [{[0] 1}]}
  ;; (show! (dot program))

  "clojure foreign interop with a value"
  (def a 1)                                                 ; value
  (def program (dataflow a))                                ; clojure interop with a value
  (def process (debug! program))
  (humanize program (heap-dump @process)) := {`a 1}
  @process := {:status :terminated
               :log    [{[0] 1}]}
  ;; (show! (dot program (heap-dump @process)))

  "clojure foreign interop to a variable"
  "deref adds a signal at the callsite. Not watch"
  ; variable is not overloaded in clojure so we can take that word
  (def !a (atom 1))
  (def a (m/watch !a))                                      ; HFDL variable (missionary flow)
  (def program (dataflow @a))                               ; clojure interop with a HFDL variable
  (def process (debug! program))
  (humanize program (heap-dump @process)) := {`a _, `@a 1}
  @process := {:status :running                             ; maintain result forever
               :log [{[0] _, [1] 1}]}                       ; @ introduced a signal
  ;; (show! (dot program (heap-dump @process)))

  "maintain result in response to variable changes"
  (swap! !a inc)
  @process := {:status :running
               :log    [{'a _ '@a 1}
                        {'@a 2}]}
  (humanize program (heap-dump @process)) := {`a _, `@a 2}
  ;; (show! (dot program (heap-dump @process)))

  "Transition between running and terminated state"
  (def !a (atom 0))
  (def a (->> (m/watch !a)
              (m/transform (take-while even?))))

  (def program (dataflow @a))
  (def process (debug! program))

  @process := {:status :running
               :log    [{[0] _, [1] 0}]}

  (reset! !a 2)
  @process := {:status :running
               :log    [{[0] _, [1] 0}
                        {[1] 2}]}

  (reset! !a 1)
  @process := {:status :terminated ; terminate process because the underlying discrete flow terminated
               :log    [{[0] _, [1] 0}
                        {[1] 2}
                        {[1] ::hfdl/free}]}

  ;; (show! (dot program (heap-dump @process)))

  "clojure foreign fn call"
  (def f inc)
  (def program (dataflow (f 1)))
  (def process (debug! program))
  @process := {:status :terminated ; terminate because there are no alive signals
               :log    [{[0] inc
                         [1] 1
                         [2] 2}]}
  (humanize program (heap-dump @process)) := {`f     inc
                                              1      1
                                              `(f 1) 2}

  ;; (show! (dot program))
  ;; (show! (dot program (heap-dump @process)))

  "clojure foreign fn call, composed"
  (def f inc)
  (def program (dataflow (f (f 1))))                        ; still no alive signals
  (def process (debug! program))
  @process := {:status  :terminated
               :log  [{[0] inc
                       [1] 1
                       [2] 2
                       [3] 3}]}

  (humanize program (heap-dump @process)) := {1          1
                                              `f         inc
                                              `(f 1)     2
                                              `(f (f 1)) 3}
  ;; (show! (dot program (heap-dump @process)))

  "clojure foreign fn call in response to variability"
  (def !a (atom 1)) (def a (m/watch !a))
  (def f inc)
  (def program (dataflow (f @a)))
  (def process (debug! program))
  @process := {:status :running
               :log    [{[0] inc
                         [1] _
                         [2] 1
                         [3] 2}]}

  (humanize program (heap-dump @process)) := {`a      _
                                              `f      inc
                                              `@a     1
                                              `(f @a) 2}
  ;; (show! (dot program (heap-dump @process)))

  "dataflow apply (reactive fn)"
  (def !f (atom +)) (def f (m/watch !f))
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (@f @a 2)))                        ; variable function in fncall position
  (def process (debug! program))
  (def render (differential-dot program))
  @process := {:status :running
               :log    [{[0] f
                         [1] +
                         [2] 2
                         [3] a
                         [4] 1
                         [5] 3}]}

  ;; (show! (render (heap-dump @process)))

  (reset! !f -)
  @process := {:status :running
               :log    [{[0] f
                         [1] +
                         [2] 2
                         [3] a
                         [4] 1
                         [5] 3}
                        {[1] -
                         [5] -1}]}

  ;; (show! (render (heap-dump @process)))

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
               :log    [{[0] b,
                         [1] 2,
                         [2] inc,
                         [3] 3,
                         [4] +,
                         [5] a,
                         [6] 1,
                         [7] 4}
                        {[1] 3, [3] 4, [7] 5}]}

  "does let introduce signals"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom 2)) (def b (m/watch !b))
  (def program (dataflow (let [a2 @a
                               b2 @b]
                           (+ a2 (inc b2)))))
  ;; TODO this program = program above
  ;; TODO this process log = process log above
  (def process (debug! program))
  (def render (differential-dot program))
  ;; (show! (render (heap-dump @process)))
  (swap! !b inc)
  ;; (show! (render (heap-dump @process)))
  @process := {:status :running
               :log [{[0] b,
                      [1] 2,
                      [2] inc,
                      [3] 3,
                      [4] +,
                      [5] a,
                      [6] 1,
                      [7] 4}
                     {[1] 3, [3] 4, [7] 5}]}

  "simplest diamond, no let"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (+ (inc @a) (dec @a))))                        ; two @
  (def process (debug! program))
  @process ;; => {:status :running, :log []}
  (show! (dot program (heap-dump @process)))
  @process := {:status :running
               :log [{'a         _
                      '@a        1
                      '+         clojure.core/+
                      '(+ @a @a) 2}]}                     ; one signal! reused twice

  "simplest diamond, no let"
  ;; This test implies the AST (two (inc @)) does not match the Viz (one (inc @))
  ;; this happens without an optimizer? Consequence of RT
  ;; what is an optimizer?
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (+ (inc @a) (inc @a))))          ; two (inc @)
  (def process (debug! program))
  @process ;; => {:status :running, :log []}
  (show! (dot program (heap-dump @process)))
  @process := {:status :running
               :log [{'a         _
                      '@a        1
                      '+         clojure.core/+
                      '(+ @a @a) 2}]}

  "same thing with let?"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow
                (let [b @a]
                   (+ b b))))                             ; this is just clojure, 100% foreign call
  (def process (debug! program))
  @process := {:status :running
               :log [{'a         _
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
  @process := {:status :running
               :log [{1       1
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
    @process := {:status :running
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
                           (vector c d))))
  (def process (debug! program))
  @process := {:status :running
               :log [{'a                   _
                        '@a                  0
                        '(inc @a)            1
                        '(dec @a)            -1
                        '[(inc @a) (dec @a)] [1 -1]}]}


  "`if` picks one branch or the other"
  (def !a (atom 1)) (def a (m/watch !a))
  (def !b (atom :b)) (def b (m/watch !b))
  (def !c (atom :c)) (def c (m/watch !c))

  (def program (dataflow @(if (odd? @a) b c)))
  (def process (debug! program))

  (def render (differential-dot program))

  ;; (show! (render (heap-dump @process)))

  (humanize program (heap-dump @process))
  :=
  {nil                                                    nil,
   `@a                                                    1,
   `get                                                   get,
   `(get (hash-map nil ~'~c false ~'~c) (odd? @a) b)      _,
   `c                                                     c,
   `@(get (hash-map nil ~'~c false ~'~c) (odd? @a) ~'~b)  b,
   `b                                                     b,
   `~'~b                                                  _,
   `odd?                                                  odd?,
   `@@(get (hash-map nil ~'~c false ~'~c) (odd? @a) ~'~b) :b,
   false                                                  false,
   `hash-map                                              hash-map,
   `a                                                     a,
   `(odd? @a)                                             true,
   `(hash-map nil ~'~c false ~'~c)                        {nil   _,
                                                           false _},
   ~'~c                                                   _}

  (swap! !a inc)

  ;; (show! (render (heap-dump @process)))

  (reset! !c :c-updated2)

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
  @process := {:status :running
               :vars  {'a 1
                       '% :c}}

  (swap! !a inc)
  @process := {:status :running
               :vars  {'a 2
                       '% :d}}

  (reset! !d :d-updated)
  @process := {:status :running
               :vars  {'a 2
                       '% :d-updated}}

  "Destructuring"

  (def !pair (atom [:a 1])) (def pair (m/watch !pair))
  (def program (dataflow (let [[a b] @pair]
                           (vector a b))))
  (def process (debug! program))
  @process := {:status :running
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


  "does dataflow compose with missionary"  ; yes
  (def !a (atom 1)) (def a (m/watch !a))
  (def program (dataflow (+ @a 2)))        ; well-typed
  (def process (debug! program))
  @process := {:status :running
               :vars  {'a        _
                       '@a       1
                       '+        +
                       2         2
                       '(+ @a 2) 3}}

  (m/cp (println (m/? program)))                 ; compose processes?
  ;; Defered until we talk about spawn


  "dataflow composes like flow"
  (def !a (atom 1)) (def a (m/watch !a))


  (defn program1 [a b] (dataflow (+ @a b)))
  (def program2 (dataflow [@(program1 a 1) @(program1 a 2)]))

  ; main goal of compiler phase in the IL is to free the interpreter from macroexpansion
  ; which requires a full clojure evaluator. We don't want to require the client to have
  ; full eval capabilities (too expensive).

  (def process (debug! program2))
  (show! (dot program2 (heap-dump @process)))
  (humanize program2 (heap-dump @process))
  @process := {:status :running
               :log    [{
                         ;nil                 2,       ; ?
                         1                   1,
                         2                   2,
                         'a                  a

                         'program1           program1

                         '(program1 a 1)     {'a  a
                                              '@a 1
                                              '+  +
                                              1   1}
                         '@(program1 a 1)    2,

                         '(program1 a 2)     {'a  a
                                              '@a 1
                                              '+  +
                                              2   2}
                         '@(program1 a 2)    3,

                         'vector             vector
                         '(vector
                            @(program1 a 1)
                            @(program1 a 2)) [2 3]}]}

  "Like flows, dataflows are RT and run once when linked twice"
  (def !a (atom 1)) (def a (m/watch !a))
  (def program1 (dataflow @a))
  (def program2 (dataflow [@program1 @program1]))
  (def process (debug! program2))
  @process := {:status :running
               :log    [{'program1              {'a a '@a 1}
                         '@program1             1
                         '[@program1 @program1] [1 1]}]}

  "X"
  (def program (dataflow @(dataflow (str 1))))
  (def process (debug! program))
  ;(select-keys (:log @process) []) :=
  @process := {:status :running
               :log    [{'(dataflow (str 1))  {1        1   ; [4 0 0]
                                               'str     str ; [4 0 1
                                               '(str 1) "1"} ; [4 0 2]
                         '@(dataflow (str 1)) "1"}]}        ; [4]

  "Ifn"
  (def plus-one (fn [x] (dataflow (inc @x))))
  (def program (dataflow @(plus-one ~1)))
  (def process (debug! program))
  @process := {:status :running
               :log    {1               1                   ; [0]
                        ~1              _                   ; [1]
                        'plus-one       plus-one            ; [2]
                        '(plus-one ~1)  _                   ; [3]
                        '@(plus-one ~1) 2                   ; [4]
                        _               {'inc      _        ; [4 0 0] -- compiled inc
                                         'x        _        ; [4 0 1]
                                         '@x       1        ; [4 0 2]
                                         '(inc @x) 2        ; [4 0 3]
                                         }}}

  "Composed ifn calls"
  (def plus-one (fn [x] (dataflow (inc @x))))
  (def minus-one (fn [x] (dataflow (dec @x))))
  (def program (dataflow @(minus-one (plus-one ~1))))
  (def process (debug! program))
  @process := {:status :running
               :log    [{1                           1      ; [1]
                         ~1                          _      ; [1]
                         'plus-one                   plus-one ; [2]
                         '(plus-one ~1)              ?ZZ    ; [3] -- dataflow
                         'minus-one                  minus-one ; [4]
                         '(minus-one (plus-one ~1))  _      ; [5] -- dataflow
                         '@(minus-one (plus-one ~1)) {'dec      _ ; [6 0 0]
                                                      'x        ?ZZ ; [6 0 1]
                                                      '@x       {'inc      _ ; [6 0 2 0 0]
                                                                 'x        _ ; [6 0 2 0 1]
                                                                 '@x       1 ; [6 0 2 0 2]
                                                                 '(inc @x) 2} ; [6 0 2 0 3]
                                                      '(dec @x) 1} ; [6 0 3]
                         '@(minus-one (plus-one ~1)) 1}]}   ; [6]

  "deref after unquote is identity"
  (def program (dataflow @~1))
  (def process (debug! program))
  @process := {:status :terminated
               :logs   [{1    1
                         '~1  _
                         '@~1 1}]}

  "unquote after deref is identity"
  ;; This test can't pass without an optimization phase canceling out join after
  ;; extend or extend after join.

  (def program (dataflow ~(deref ~1)))                      ; careful ~@ is unquote-splice
  (def process (debug! program))
  @process := {:status :running
               :log    [{1            1
                         '~1          ?a
                         '@~1         1
                         '~(deref ~1) ?a}]}
  )
