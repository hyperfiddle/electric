(ns dustin.y2022.missionary-dag-supervision
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

(comment
  (def i (m/sleep 1000 42))
  '(- (inc i) (dec i))

  (def x (m/sleep 1000 6))
  (def y (m/sleep 1000 7))

  (p/defn third [a b c] c)

  '(let [b [x y]]
     (concat
       [(inc x) b]
       [b (dec y)]))


  (defn node [delay & deps]
    (m/cp (m/amb nil (m/? (m/sleep delay (m/? (apply m/join vector deps)))))))

  (m/cp
    (let [a (node 1)
          b (share (node 1))
          c (node 100)
          d (node 100 a b)
          e (node 1   b c)
          f (node 1   d e)]
      (m/?< f)))

  ; 1
  (m/sp
    (let [b (m/? (m/join vector x y))]                      ; introduces causality/ordering that is not present in the DAG
      (m/? (m/join concat
                   (m/sp (m/join vector (m/sp (inc (m/? x))) b))
                   (m/sp (m/join vector b (m/sp (dec (m/? y)))))))))

  ; 2
  (m/sp
    (let [>b (future! (m/? (m/join vector x y)))]           ; allocate buffer, don't eval
      (m/? (m/join concat
                   (m/sp (m/join vector (m/sp (inc (m/? x))) >b))
                   (m/sp (m/join vector >b (m/sp (dec (m/? y)))))))))

  ; 3
  ;; not clojure, just a symbolic representation of the dag
  (quote
    (let [b [x y]]
      (concat [(inc x) b] [b (dec y)])))

  (defn node [n f & args]
    (m/sp (m/? (m/sleep n (m/? (apply m/join f args))))))

  (def >x (m/sp 6))
  (def >y (m/sp 7))
  (defn a [>x] (node 1000 inc >x))
  (defn b [>x >y] (node 1 vector >x >y))
  (defn c [>x] (node 1 dec >x))
  (defn d [>x >y] (node 1 vector >x >y))
  (defn e [>x >y] (node 1000 vector >x >y))
  (defn f [>x >y] (node 1 concat >x >y))


  ; this version has supervised F but no sharing
  (def dag
    (m/sp
      (let [>b #_(publish-task!) (b >x >y)]
        (m/? (f (d (a >x) >b)
                (e >b (c >y)))))))

  ; Broken supervision tree
  (def dag
    (m/sp
      (let [!>b (future! (b >x >y))]                        ; we broke the tree; the tree stops at >b
        (m/? (f (d (a >x) (join !>b))
                (e (join !>b) (c >y)))))))

  (time (m/? dag)) := (7 [6 7] [6 7] 6)                     ;; runs in 1 second

  ; Problem:
  ; if we cancel `dag` (m/sp process); the cancel signal stops at >b and oprhans
  ; we cancel the future's await but not the future's process (task)

  "Fiber definition"
  (defprotocol Fiber
    (cancel [_])
    (join [_]))

  (defn future! "Runs a task immediately and returns a fiber. Cats effect : start; ZIO : fork"
    [task]
    (let [state (m/dfv)
          cancel (task #(state (fn [] %))                   ; here task is orphaned from supervision tree
                       #(state (fn [] (throw %))))
          listen (m/sp ((m/? state)))]
      (reify Fiber
        (cancel [_] (cancel))                               ; cancel must be called manually
        (join [_] listen))))                                ; task that awaits future


  ; Refactor to match the flow protocol and hide the Fiber
  (defn publish-task! [task]
    (let [state (m/dfv)                                     ; state can be hoisted into reactor - via dynamic scope side effect
          cancel (task #(state (fn [] %))
                       #(state (fn [] (throw %))))
          listen (m/sp ((m/? state)))]
      (fn
        ([] (cancel))                                       ; this arity is not understood by missionary supervisor!!! which is the problem
        ([s f] (listen s f)))))



  ; Still broken supervision tree, but refactored to match missionary flow interface to avoid the explicit fiber join.
  ; this version has sharing, but loses supervision on F
  (def dag
    (m/sp
      (let [>b (b >x >y)                                    ; supervisison but no sharing
            >b' (publish-task! >b)]                         ; sharing but no supervision
        ; no longer supervised by m/sp
        (m/? (f (d (a >x) >b')                              ; can use >b or >b' for one or the other
                (e >b' (c >y)))))))

  ; What if you call publish-task! but never use it?
  ; You've allocated a buffer and it is never freed
  ; Thus, publish-task! is broken and there's no way to fix it.

  ; L: This can be solved, RX does solve this (but doesn't solve the glitch problem)
  ; RX has a refcount for pubishers
  ; What we need is both glitch-free and refcount.



  ; L: cats effect and ZIO have the same problem; when you call fork you need to be careful of memory leaks

  )

(comment
  "Proposed solution"

  ; Leo now proposes an operator for publishing a task or flow
  ; it doesn't start the process immediately, it starts on first subscription.
  ; When the last subscription is cancelled, the process is cancelled.
  ; Unless, it's terminated in which case the result is memoized and reused for future flows.

  ; Solution Shape = lazy and memoized.
  ; missionary/task is lazy and not memoized
  ; clojure.core/future is eager and memoized and blocks thread
  ; clojure.core/delay is lazy and memoized but blocks thread

  ; Does this fn exist in Clojure?
  ; we have a fn that does the same as clojure.core/future but lazy - clojure.core/delay
  ; we do not want that because it relies on blocking threads
  (def !x (delay (fib 100)))
  @!x                                                       ; block the first caller thread to run the delay
  ; to cancel, must kill the current thread and then the delay would memoize the interrupted exception
  (def m (delay (Thread/sleep Long/MAX_VALUE)))
  (def h (future @m))
  (future-cancel h)                                         ; interrupt the thread
  ; reading the future rethrows the memoized exception state
  (type @m) throws? TheadInterruptException


  ; we want async delay, with supervision.
  ; After (publisher-cancel!) which is analogous to (future-cancel) above,
  ; refcount is zero and we want to reset the buffer to a clean slate so that it can be subscribed to again.

  ; L: also, if we have two subscribers and we cancel one, we don't want to cancel the process.
  ; programs are values, processes are never values (can be ID). "Process" is always interop

  (def dag
    (m/ap
      (let [>b (b >x >y)
            >b' (m/stream >b)]                              ; no ! and note this is stateful object
        (m/? (f (d (a >x) >b')
                (e >b' (c >y)))))))

  (def !it (dag #(prn ::notify) #(prn ::terminate)))
  ; no need for reactor, because no buffer is allocated and no processes spawned until subscribed to.
  ; therefore no need to hoist mutable state through dynamic scope
  ; the subscribers to >b' will increment the refcount by side effect of subscribing.


  ; We still need reactor logic, because we need to implement propagation algorithm between publishers
  ; (what RX doesn't do), but the entrypoint cannot be the same because the supervision is now performed by
  ; the subscribers.




  ; Final Q: why must user indicate if they want or don't want sharing?
  ; L: you want to input text in a form. you don't want them to be shared, you really want two different inputs with
  ; different object identity. a compiler could try to share them but you want two user inputs.

  (def !x (atom ""))
  (def !y (atom ""))

  ; Show me a use case when this is the better program (due to side effects?)
  ; we've asked to run the b node twice for effect. and not share.
  (defn b [>x >y] (node 1 (fn [& args]
                            (println `effect args)
                            (apply vector args)) >x >y))
  (def dag
    (m/ap
      (let [>b (b >x >y) #_#_>b' (m/stream >b)
            >b2 (b >x >y)]
        (m/? (f (d (a >x) >b)
                (e >b2 (c >y)))))))

  ; you never want to skip the effect of sleep
  (let [a 1
        >x (m/sleep 1 1)]
    (m/sp (let [>x' (m/sleep 1 1)]
            (m/join vector >x >x)                           ; sleep twice
            (m/join vector >x' >x'))))                      ; sleep once

  ; async await, no sharing
  (m/sp (let [>x' (m/sleep 1 1)
              x (m/? >x')]                                  ; sleep once
          (vector x x)))

  (m/sp (let [>x' (m/sleep 1 1)]
          (vector (m/? >x') (m/? >x'))))                    ; sleep twice


  (defn counter []
    (m/ap (loop [x 0]
            (m/amb x
                   (if (pos? x)
                     (do (m/? (m/sleep 100))
                         (recur (inc x)))
                     x)))))

  (tests
    "no m/latest, direct fn call with two switches"
    (tests
      (def !x (atom 0))
      (def !it ((m/cp (let [<x (m/watch !x)
                            x (m/?< <x)]                    ; countdown booted once
                        (vector x x)))
                #() #()))
      @!it := [0 0]
      (swap! !x inc)
      @!it := [1 1]
      (!it))

    (tests
      (def !x (atom 0))
      (def !it ((m/cp (let [<x (m/watch !x)]
                        (vector (m/?< <x) (m/?< <x))))      ; two watches but we don't notice. parent/child
                #() #()))                                   ; to observe the glitch - sample in the flow's ready signal (between watch callbacks)
      @!it := [0 0]
      (swap! !x inc)
      @!it := [1 1]
      (!it))


    (tests
      ; stream samples cp as fast as it can
      (def !x (atom 0))
      (def !it ((m/reactor (m/stream! (m/cp (let [<x (m/watch !x)]
                                              (! (vector (m/?< <x) (m/?< <x))))))) ; two watches
                prn prn))
      % := [0 0]
      (swap! !x inc)
      % := [0 1]                                            ; glitch
      % := [1 1]
      (!it))

    (tests
      (def !x (atom 0))
      (def !it ((m/reactor (m/stream! (m/cp (let [<x (m/signal! (m/watch !x))]
                                              (! (vector (m/?< <x) (m/?< <x))))))) ; one watch
                prn prn))
      % := [0 0]
      (swap! !x inc)
      % := [1 1]
      (!it)))

  (tests
    "same thing, but with m/latest"
    (tests
      (def !x (atom 0))
      (def !it ((m/cp (let [<x (m/watch !x)]                ; countdown booted once
                        (m/?< (m/latest vector <x <x))))    ; different tree, they are siblings
                #() #()))
      @!it := [0 0]
      (swap! !x inc)
      @!it := [1 1]
      (!it))
    ; no dynamism here, faster.

    ; second test is redundant, why?

    (tests
      ; stream samples cp as fast as it can
      (def !x (atom 0))
      (def !it ((m/reactor (m/stream! (m/cp (let [<x (m/watch !x)]
                                              (! (m/?< (m/latest vector <x <x))))))) ; two watches
                prn prn))
      % := [0 0]
      (swap! !x inc)
      % := [0 1]                                            ; glitch (faster and in parallel)
      % := [1 1]
      (!it))

    (tests
      (def !x (atom 0))
      (def !it ((m/reactor (m/stream! (m/cp (let [<x (m/signal! (m/watch !x))]
                                              (! (m/?< (m/latest vector <x <x))))))) ; one watch
                prn prn))
      % := [0 0]
      (swap! !x inc)
      % := [1 1]
      (!it)))

  ; what we want is:
  ; sharing (one watch); concurrent; lazy
  ; and we agree the supervision tree is DAG
  ; only question is what is the default - sharing or not sharing.

  ; Leo: or a mixed soln, where flows like watch are implicitly shared.
  ; D: continuous flows are always shared? (Don't sleep in CT)

  ; 3 cases
  ; 1 - sharing is explicit
  ; 2 - sharing by default - tasks are stateful
  ; 3 - sharing by default - change meaning of let in m/cp
  ;
  ; or, a mix of both. (contract of missionary operator to decide if sharing or not)
  ; ... e.g. watch is shared, sleep is not shared

  (comment
    ; 1 - sharing is explicit
    (def !x (atom 0))
    (def !it ((m/cp (let [<x (m/signal (m/watch !x))]       ; explicit share, no reactor now
                      (! (m/?< (m/latest vector <x <x)))))  ; one watch
              prn prn))
    % := [0 0]
    (swap! !x inc)
    % := [1 1]
    (!it))

  (comment
    ; 2 - sharing by default - tasks/flows are stateful
    (def !x (atom 0))
    (def !it ((m/cp (let [<x (m/watch !x)]                  ; implicit share on flow object <x used twice
                      (! (m/?< (m/latest vector <x <x)))))  ; one watch
              prn prn))
    % := [0 0]
    (swap! !x inc)
    % := [1 1]
    (!it))

  (comment
    ; 3 - sharing by default - change meaning of let in m/cp
    (def !x (atom 0))
    (def !it ((m/cp (let [<x (m/watch !x)]                  ; implicit share on let binding in m/ap
                      (! (m/?< (m/latest vector <x <x)))))  ; one watch
              prn prn))
    % := [0 0]
    (swap! !x inc)
    % := [1 1]
    (!it))

  ; 2 and 3 are same syntax in this example, different mechanism
  ; D: Hypothesis: in CT you always want to share; which means mechanism doesn't matter

  ; Note - if always sharing, the compiler can rewrite (m/latest veector (m/watch !x) (m/watch !x))
  ; to singleton instances of each flow

  ; How do you opt-out of sharing?
  (comment
    ; this is the same in both 2 and 3 machinery (let vs stateful flow)
    (def !x (atom 0))
    (def !it ((m/cp (let [<x (m/watch !x)
                          <x2 (m/watch !x)]
                      (! (m/?< (m/latest vector <x <x2)))))
              prn prn))
    % := [0 0]
    (swap! !x inc)
    % := [1 1]
    (!it))

  (comment
    (def !x (atom 0))
    (def !it ((let [<x (m/watch !x)]
                (m/cp (! (m/?< (m/latest vector <x <x)))))  ; glitch - two watches (bad)
              prn prn))
    % := [0 0]
    (swap! !x inc)
    % := [1 1]
    (!it))

  ; L: do we share by default or have RT by default? L: Not sure
  ; share by default means you need to refactor/inline effects to make them run twice
  ; RT by default means you need an extra operator for sharing.

  ; L: it's a practical decision - not about power, it's about usage and convenience
  ; Not just about removing the operator, it also changes how you reason about your application

  ; How to implement sharing by default?
  ; 1 - use the let special form (hard/impossible to impl with dynamic types, also hard to explain)
  ; 2 - make the tasks stateful

  ; There are multiple ways to share an effect
  ;  for tasks - there is only 1
  ;  for flows - there are signal! and stream!
  ; sharing by default means there is no ambiguity - all flows are unambiguously signals or streams



  (def >sleep1 (m/sleep 100))
  (m/ap (loop [x x]
          (m/amb x
                 (if (pos? x)
                   (do (m/? >sleep1)
                       (recur (dec x)))
                   x)))))
; if shared, it will only sleep once

; Suppose all tasks are shared by default:
(defn SleepObject [] (m/sleep 100))
(m/ap (loop [x x]
        (m/amb x
               (if (pos? x)
                 (do (m/? (SleepObject))                    ; inline to get many sleeps
                     (recur (dec x)))
                 x))))

; What are the costs of sharing by default (loss of RT)?
; syntax - if tasks have identity - need factories instead of publishers,
;          or let bindings can create identity (so tasks remain values)
; Leo doesn't like using `let` for identity because it changes the meaning of let.
;   in what way?
;       it means let in ap is different than let in clojure

(def >sleep1 (m/sleep 100))
(m/ap (loop [x x]
        (m/amb x
               (if (pos? x)
                 (do (m/? >sleep1)
                     (recur (dec x)))
                 x))))

; if everything is implicitly shared, you have to care about when you define the tasks
; and flows.




; Leo concludes

'(let [b [x y]]
   (concat
     [(inc x) b]
     [b (dec y)]))

; Because it's not always a pure optimization, it can change meaning when there is effects.
; if the computation is pure you can always share.
; if there is effects, you want to control when and how many times they are run.




; async await?
(let [a 1
      >x (m/sleep 1 1)]
  (m/sp (let [>x (m/sleep 1 1)]
          (m/join vector >x >x)                             ; sleep twice
          (m/join vector >x' >x'))))                        ; sleep once









(comment
  "Dustin's idea"

  ; 3
  (defn fapply [f ma mb & mxs])
  (defn fmap [f mx])
  (defn bind [f mx])

  ; if 3 is late, it prevents 1 and 2
  (lazy
    (fapply (fn [%1 %2 %3]
              (fapply mconcat
                      (fapply mvector (pure %1) (pure %2))
                      (fapply mvector (pure %2) (pure %3))))
            (fmap inc x) b (fmap dec y)))

  (lazy
    (mlet [[%1 %2 %3] <- (fapply (fmap inc x) b (fmap dec y))
           [_ _] (fapply mconcat)
           _ <- (fapply mvector (pure %1) (pure %2))
           _ <- (fapply mvector (pure %2) (pure %3))

           ]

          )
    )
  ) )