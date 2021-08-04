(ns dustin.missionary6_docs
  "documentation end to end for missionary"
  (:require
    [minitest :refer [tests]]
    [missionary.core :as m :refer [? ?? ?! ap sp]]
    [dustin.missionary5_reactor :refer [sleep-emit]]))

(tests
  "task"

  ;
  ; tasks are a value
  (def task (m/sleep 1000))
  ; they are RT
  ; https://github.com/leonoel/task

  ; run or "ask" the task
  ; for it's effect
  ; ask will block the current process until value is available
  ; it's a sequential thing. Like deref on future
  (? task)
  (? )
  ; Blocking here is just a REPL convenience. In a correctly written program, you don't need this!

  ; you can always run a task by calling it as a function with the success and failure callbacks as args
  (task (partial prn :success) (partial prn :failure))
  ; :ready
  ; nil

  ; Task is a protocol / convention, it's not a concrete type
  #_(fn [s f] ...)
  ; you can pass this to the JS Promise constructor

  ; Calling task as a fn returns a 0-arg control fn you can use to cancel the task
  (def cancel ((m/sleep 100000) (partial prn :success) (partial prn :failure)))
  ; long wait
  (cancel)
  ; calls failure callback, because that's how sleep works
  ; prints
  ; :failure ...
  ; nil

  ;(task (prn :ready) (prn :done)) ; corrupts scheduler

  (println 1)


  ; SP is for sequential composition.
  ; stands for sequential process
  ; By "sequential compsition" we mean, effects compose by running them in order
  ; it's an alternative to monadic composition in IO
  ; it has exception short circuit like IO
  ; Task is the same kind of object as haskell's IO
  ; except Task doesn't have a type because we are in lisp
  ; task :: (v -> IO) -> (err -> IO) -> (-> IO)




  ; Sequential composition
  ; this is like monad composition, and returns a new task-value which runs the composition
  (def task (sp (let [x (? (m/sleep 1000 :x))]
                  (println x)
                  x)))

  ; if ask is inside sp/ap block, it is desugared to a continuation
  ; if outside, it blocks the current thread
  ; This is a design choice. Core.async made a different choice, they have two operators -
  ; block thread has an extra bang



  ; can run the task
  (? task)

  ; can run again, it's reusable value
  (? task)

  ; Reactor does not help tasks, because nodes in a reactor are flows.
  ; But tasks are still convenient because they can be combined with flow
  ; operations in ap blocks

  (defn sleep' [delay v]
    (m/sp
      (let [x (m/? (m/sleep delay v))]
        (println x)
        x)))

  (defn sleep' [delay v]
    ; compiler phase inline - delay and v are in scope still
    ; after both sp and cr macroexpansions
    (cr {} (let [x (? (m/sleep delay v))]
             (println x)
             x)))

  (sleep' 1000 :x)
  ; := #object[clojure.core$partial$fn__5824 0x7434f689 "clojure.core$partial$fn__5824@7434f689"]
  ; The s-expression is gone, it's wrapped in a thunk


  ; monad-like composition with dynamic continuation blocks
  (sp (let [a (? (sleep' 1000 42))]
        (? (if (odd? a)
             (sleep' 1000 a)
             (sleep' 1000 (inc a))))))
  ; there are two sp macros now
  ; "the second one is referring to the first"
  ; the inner sp has been compiled, so the outer SP cannot understand the inner SP

  ; these sleeps are sequential and thus take two seconds!
  (sp (vector
        (? (m/sleep 1000 :a))                               ; ? will park
        (? (m/sleep 1000 :b))))

  ; This is really a Clojure problem, could just use two Thread/sleeps for this

  ; how to fix this?
  ; let's talk about naive soln first
  ; we want to parallelize the sleeps. There's an operator for that
  (def task
    (m/join vector                                          ; runs in 1 seconds
      (m/sleep 1000 :a)
      (m/sleep 1000 :b)))
  ; join expects tasks, so don't need sp macro
  ; sp macro is about sequential composition and this is now parallel!

  ; This is no longer a purely sequential process
  ; (Tasks can run in parallel)

  (? task)
  ; delay one second
  := [:a :b]


  (defn f [xs] (map str xs))
  (? (sp (f (? task))))
  ; delay one second
  := '(":a" ":b")

  ; errors work


  (let [x-task (m/sleep 1000 :x)]
    (m/join vector x-task x-task))

  ; sleep is a RT operation, so join does not see that
  ; these are the same task, the task should run once and the result reused



  ; Above example is still purely hierarchical
  ; lets's say we have a complex task, it's computing an expensive value

  (defn fib [x]                                             ; something slow
    (m/!)
    (cond
      ;(Thread/interrupted) (throw (ex-info {}))
      (= x 0) 1
      (= x 1) 1
      () (+ (fib (dec x))
           (fib (dec (dec x))))))
  (map fib (range 10))
  := '(1 1 2 3 5 8 13 21 34 55)

  (fib 37)                                                  ; slow, 1000ms




  (let [e (java.util.concurrent.Executors/newSingleThreadExecutor)]
    (defn fib-async [n]
      ; via turns a blocking op into an async op via an executor
      ; cpu is a fixed threadpool bound to number of cpus available
      (m/via e
        #_m/cpu
        (println 'effect)
        (fib n))))
  := 39088169
  (time (? (m/timeout 500 (fib-async 37))))
  ; effect
  ; timeout


  (time (? (m/timeout 3000 (m/join vector (fib-async 37) (fib-async 37)))))
  ;effect
  ;effect
  ;"Elapsed time: 2914.069509 msecs"
  := [39088169 39088169]
  ; because tasks have no shared identity. This is not a DAG!

  (time (? (m/timeout 5000
             (let [x-task (fib-async 37)]
               (m/join vector x-task x-task)))))
  ; Still not a dag!


  ; We can cheat by only computing the result once
  (def task
    (sp (let [x (? (fib-async 37))]                         ; compute once
          (? (m/join vector                                 ; join is parallelism
               (m/sleep 100 x)                          ; parallel sleep
               (m/sleep 101 x))))))

  ; we create a dag, for the fib
  ; then that finishes, we create a new dag (join vector sleep sleep)
  ; then that finishes, those processes terminate with result and are GC'ed
  ; all dags are GC'ed now

  ; But here the DAG is actually always a tree !!!
  ; because of RT
  ; Lets try to make this a DAG and discover we can't
  ; there is not a single point in time where a process has more than one parent
  ; we have a tree and the topology of the tree varies over time but it's always a tree


  ; sleep is not a child of fib



  (defn foo [props] [div {:class (:class props)}])
  (defn foo [props] `(div ~{:class (:class props)}))
  (defn foo [props] (div {:class (:class props)}))
  (defn foo [props]
    (via (div {:class (:class props)}
           (sleep 1000 "hello")
           (sleep 1001 "world"))))
  (foo {}) := [:div "hello" "world"]

  (foo {:class "red"}) := [:div {:class "red"}]






  (time (? (m/timeout 5000 task)))
  ;effect
  ;"Elapsed time: 1666.768893 msecs"
  ;=> [39088169 39088169]

  ; Effect was run only once (which is not parallelism, we cheated)
  ; With tasks we can usually cheat in this way, but with flows
  ; it becomes much more complicated
  ; usually or always? Not sure

  ; There is a hierarchy to the process
  ; the hierarchy reflects the hierarchy in the AST
  ; A very important property as it gives natural way to flow for
  ; exceptions and cancellation
  ; here,
  ; Sleep is parented at join, and join is parented at sp
  ; thus if sleep terminates (e.g. exception), it will signal upward
  ; It's bi-directional – if sp cancels externally, it will signal down to sleep


  ; Dynamic topology
  ; Dynamic process hierarchy

  ; Operating system has program, process and effects, termination and cancellation

  ; A dag is different than a hierarchy

  ; cancellation


  ; The real answer to fib is to define a SP that computes the fib result
  ; and only after that call the join
  ; the missionary answer is to arrange expensive computations by hand
  ; our use our dataflow macro to assume the computation is RT and thus incrementally maintain




  ; Another (heavy) solution to this problem is "Reactor"
  ; There are simpler ways to fix this than reactor
  ; for more complex use cases, reactor makes sense

  ; Note can always turn a task into a flow with ap
  ; tasks are strictly more powerful
  )

; Two kinds of flows, discrete and continuous

(tests
  "discrete flows"

  ; a task is a program that eventually produces one value
  ; a flow is a program that eventually produces multiple values
  ; "eventually" just means async, it's backed by IO so we don't know when the
  ; values are reproduced


  ; https://github.com/leonoel/flow
  ; prior art is why reactive-streams sucks
  ; reactive-streams is the team behind reactiveX, a popular java reactive framework
  ; akka team helps

  ; we aren't 100% sure of the goals of reactive streams
  ; officially, it's a collaborative effort to standardize the reactive ecosystem in java
  ; it's been adopted and integrated in the official JDK
  ; it has support from java core team
  ; it's not clear what is the actual motivations ... it seems it's a standardization of
  ; RX bad engineering

  ; RX is
  ; writing RX in java is really painful because no good metaprogramming support
  ; cannot implement coroutine syntax like in clojure for example
  ; the composition code is necessarily ugly ... flatmap async etc.
  ; better with java lambdas but still really painful

  ; java team just thinks standards are good it will make

  ; AP is for flow composition , like SP
  (ap 42)

  ; the thesis of continuous flows is that some computations are RT (other than
  ; compute resources consumed) and we just want to incrementally maintain them to make
  ; them update faster as an optimization
  ; this is wrong, you can have eager incremental maintenance
  ;
  ; examples: reactjs, spreadsheets, database queries
  ;
  ; Leo says this is technically true but not sure how it helps
  ;
  ; Leo's words:
  ; you want to sample lazily because you don't know in advance when (at which point in time)
  ; the value will actually be needed
  ; the computation of the dag should be triggered by the consumer

  ; Is incremental a properly defined word? or just Jane Street? I dont know
  ;
  ;  continuous flows make sense if each new value invalidates the previous one
  ; no history sensitivity
  ; no

  ; try again
  ; the thesis of continuous flows is that it is a special optimzation case of discrete flows
  ; where each new value invalidates the previous one (e.g. there is no history sensitivity).
  ; that means you can defer computation until the value is asked for.
  ;    reactjs is discrete, but could actually be faster if continuous?
  ;    it depends how you use it ... this is the confusion

  ; ReactDOM.render()
  ; this attaches a reactor to the document and runs forever ... its all about effects
  ; manage the effect of the dom patches
  ; manage the effects of computation of HTML view values
  ;    incremental maintenance with prevProps memoization
  ; feeding the dom events into user logic
  ; reactjs has a component lifecycle

  ; React is using an unclear approach
  ; shouldComponentUpdate
  ; this is made mandatory by the impedance mismatch between FP and Javascript

  ; therefore we learn that Reactjs has a lot in common with continuous flows
  ; This is why FRP failed to gain traction in frontend dev -
  ; discrete FRP is not what you want for frontend dom rendering

  ; the value prop of react is to provide functional programming interface to the dom
  ;
  ;

  ; so twitter is history sensitive as it counts the number of like commands,
  ; so it must be discrete


  ; RX latest same thing as missionary latest ... runs fn each time an input changes
  ; but it's fully eager because underlying is eager so it produces glitches.
  ; you can't represent the scenario where
  ;     two values change at the same time so the fn should only be run once
  ; ^ that's the glitch we want to avoid.


  ; if SP composes sequentially like IO monad
  ; how does AP compose?
  ; it's an async list monad, backed by IO instead of computations
  (ap (println (?! (m/enumerate [1 2 3]))))
  (for [x (m/enumerate [1 2 3])] (println x))               ; pseudocode (not async)
  (mapcat println (m/enumerate [1 2 3]))                    ; pseudocode (not async)

  ; This is one way to think about flows
  ; Leo: it is better to first explain what a flow is
  ; and then how to compose them.

  ; aslo 3 possible bind


  ; ambiguous process
  ; combine task and flow
  (defn emit-sleep [delays]
    (ap (let [x (?! (m/enumerate delays))]
          (? (m/sleep x ::slept)))))

  ; composes dynamically like tasks
  (ap (let [a (?? (emit-sleep [301 302 303]))]
        (?? (if (odd? a)
              (emit-sleep [a])
              (emit-sleep [(inc a)])))))

  ; AP is a sequential computation lifted into a list
  ; because each form can have more than one result
  ; AP is still sequential unless you use ?= operator which introduces parallelism
  ; the resulting flow is sequential, all parallel branches are gathered (they are racing
  ; for the final ordering in the result flow)
  )

; Aggregate also is not a primitive we use in real world. Just for testing

(tests
  "flow diamond case is interesting"
  ; illustrate the motivator for reactor

  ; we need a reactive computation where some branches are expensive



  (def >needle (m/watch !needle)) (def !needle (atom "alice"))
  (def >open (m/watch !open)) (def !open (atom true))

  (def flow
    (ap
      (println                                              ;need effect for this to make sense
        (let [needle (?! >needle)]
          (vector
            (?! (submissions needle))
            (if (?! >open)
              (?! (submissions-detail "tempid"))
              ::nothing))))))

  ; ap has no parallelism
  ; m/latest is the operator that introduces parallelism
  ; because flows are RT, intermediate computations are not reused
  ;   (bc this requires internal state shared across calls)
  ; this is the same problem as with tasks

  ; without reactors, at runtime the computation will always be a tree

  (let [e (java.util.concurrent.Executors/newSingleThreadExecutor)]
    (defn submissions' [n]
      ; via turns a blocking op into an async op via an executor
      ; cpu is a fixed threadpool bound to number of cpus available
      ; make it async, returns a task value
      (m/via e
        #_m/cpu
        (datomic.api/q ...))))
  ; same as ap basically

  (defn submissions! [needle] (d/q ... needle))
  (defn submissions [needle] (m/via _ (d/q ... needle)))
  ; does this do the effect or return a value that performs the effect
  (defn submissions-detail! [needle] (d/q ... needle))
  (defn submissions-detail [needle] (m/via _ (d/q ... needle)))

  (def flow
    (m/reactor
      (ap
        (println
          (let []                        ; fork
            (m/latest vector
              ; side effects in ap blocks must not block thread! prinln is ok. fib is bad!
              ; missionary assumes ops are nonblocking
              #_(ap (submissions! (?! >needle)))              ; blocks thread for everyone
              #_(ap (submissions! (?! >needle)))              ; blocks thread for everyone
              (ap (? (submissions (?! >needle))))           ; parallel execution
              (ap (? (submissions (?! >needle))))           ; parallel execution



              (ap
                (let [open (?! >open)]
                  (? (m/via _ (if open
                                (submissions-detail! needle)
                                ::nothing)))))

              (ap
                (if (?! >open)
                  (?! (submissions-detail "tempid"))
                  ::nothing))))))))

  (def flow
    (m/latest (comp println vector)
      (ap (? (submissions (?! >needle))))
      (ap (if-let [open (?! >open)]
            (? (submissions-detail open))
            ::nothing))))

  ; cant do this, it doesn't make sense
  ; -- (? flow)
  ; because flows are something that produce multiple values
  ; and this notation suggests we are waiting on a single value

  ; So all flows are started like this:
  (def !out (flow #(prn :notify) #(prn :terminate)))
  ; :notify
  ; the flow is now running

  @!out := _

  (reset !needle "charlie")
  ; :notify
  @!out := _

  (reset !open false)
  ; :notify
  @!out := _

  ; SO this is fast without reactor
  ; so we need a new example to add reactor



  (def flow
    (dataflow
      (println                                              ;need effect for this to make sense
        (let [needle (<- >needle)
              open (<- >open)]
          (vector
            (submissions needle)
            (if open
              (<- (submissions-detail "tempid"))
              ::nothing))))))

  (reset! >open false)
  ; reset will restart the continuation/fiber from the point
  ; where we listen to the atom

  (reset! >needle "bob")
  ; this will restart the fiber from (?! >needle) and compose sequentially from here
  ; thus recomputing the popover as well even though the popover did not change.

  ; this does not optimize because the parallelism is not explicit

  ; reactor is the difference between sequential and incremental
  ; the reactor
  ; adds internal state to the flow (across input changes over time) so that past values of nodes can be reused
  ; you assign identities to nodes so as to not recompute the whole chain


  ; create a diamond shape
  (m/ap
    (let [x (m/?< (m/seed (range 3)))]
      (vector (inc x) (dec x))))




  ; This is an IO recipe for a async stream
  ; If you were to run it you get the effect
  ; but no internal state is shared between runs! It is RT

  ; the reactor lets you share state across runs to incrementally maintain


  (reset! >open false)
  ; This is composition has no parallelism
  ; without a reactor
  ; we need to name the reuse points and create a DAG


  ; reactive fib
  (defn fib [x]
    (m/!)
    (cond
      (= x 0) 1
      (= x 1) 1
      () (+ (fib (dec x))
           (fib (dec (dec x))))))



  (let [e (java.util.concurrent.Executors/newSingleThreadExecutor)]
    (defn fib-async [n]
      ; via turns a blocking op into an async op via an executor
      ; cpu is a fixed threadpool bound to number of cpus available
      (m/via e
        #_m/cpu
        (println 'effect)
        (fib n))))

  )








(tests
  "Reactor"
  ; Reactor is about reusing processes and assigning
  ; an identity to a process in order to reuse it

  ; It doesn't make much sense in tasks ?
  (m/reactor
    )

  )



(tests
  "reactor"

  ; Why do we need a reactor?
  ; If you only have flows, you can only represent hierarchical processes
  ; The topology is a hierarchy because that's how function composition works
  ; With only flows you can't have DAG topologies

  (vector ~x ~x)                                            ; simplest dag
  ; This is not a dag because the two branches of vector are not run in parallel



  (ap


    (let [x (?? (emit-sleep [1 2 3]))]
      (?)))

  )

(tests
  "continuous flows")
