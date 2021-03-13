(ns dustin.dataflow7)

; Teach Dustin HFDL

(declare tests )

(require '[hfdl :refer [dataflow]])


(tests
  (macroexpand '(dataflow (+ a b)))
  ; opaque like a lambda
  := _)

(test
  ; these must be defined statically
  "variability"
  "incremental computation"
  (def !a (atom 1))
  (def a (m/watch !a))                                      ; returns flow
  (def program (dataflow (+ @a 2)))                         ; deref turns flow into HFDL variable
  (def process (debug! program))                            ; Process is running for effect like an OS process
  @process := {:state :running :singal-heap {'% 3 'a _}}    ; just a heap dump

  "dataflow apply"
  (def !f (atom +))
  (def f (m/watch !f))
  (def program (dataflow (@f @a 2)))                        ; variable function in fncall position
  (def process (debug! program))
  @process := {:state :running :singal-heap {'% 3 'a _}}

  (reset! !f -)
  @process := {:state :running :singal-heap {'% -1 'a _}}

  "diamond"
  (def !a (atom 0))
  (def a (m/watch !a))


  ; What is let? let is a way for the programmer to name a expression component
  (def program (dataflow (let [b @a
                               c (inc b)
                               d (dec c)]
                           (vector c d))))

  := ((fn [a] (vector (inc a) (dec a))) @a)

  (let [b (inc a)
        c (inc a)
        d (inc a)]
    )

  (fn ^:ifn [a :! Flow]
    (let [a! (m/signal 'a (inc a))
          b! (m/signal 'b (inc b))
          c! (m/signal 'c (inc c))]
      ...))

  ;(let [% (inc (inc (inc a)))] ...)

  ;'(let [x (fib @a)] (vector x x))
  ;'(vector (fib @a) (fib @a))

  (def process (debug! program))
  @process := {:state :running
               :heap  {'a 0                                 ; inputs appear in the heap
                       'b 0
                       'c _
                       'd _
                       '% [1 -1]}}
  ; Leo likes this test case, no promises. Keys of the heap should be technical IDs and binding in separate slot
  ; what's a binding? The names of the let

  (-> @process :heap count) := 5                            ; Leo thinks this is OK

  ; Leo insight triggered by "inputs appear in heap":
  ; Deref in process returns snapshot of variable state ..maybe the bindings with
  ; the original AST could be exposed with dedicated methods of the process type
  ; ...the point is it's not mutable so it doesn't have to be in deref but it should be exposed anyways
  ; ... its useful debug info ... how do I know that a given slot frame matches a specific point in the AST

  )
