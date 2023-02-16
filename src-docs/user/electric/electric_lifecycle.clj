(ns user.electric.electric-lifecycle
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m]))


(hyperfiddle.rcf/enable!)

; Missionary flows, and transitively Electric Clojure flows, have an object lifecycle (mount/unmount in React terms).
; Reactive computations have a "boot" phase (the expensive initial compute). Once booted, flows are incrementally
; maintained (a small change to an input results in a small adjustment to the output). Eventually, the program will
; terminate gracefully, terminate with error (crash), or be cancelled from the outside.

; Lifecycle hello world is implementing m/watch, which subscribes to an atom:

(defn MyWatch "a watch is a signal derived from observing changes to an atom"
  [!x]
  (->> (m/observe                                           ; missionary primitive for bridging a stateful object
         (fn constructor! [send!]                           ; mount
           ; in ctor, setup the subscriptions to the atom
           (add-watch !x ::MyWatch (fn [k ref old new]
                                     ; bridge each atom change event to the signal
                                     (send! new)))
           (send! @!x)                                      ; bridge the initial value during construction
           (fn destructor! []                               ; unmount
             (remove-watch !x ::MyWatch))))                 ; unsubscribe and release resources
       (m/relieve {})))                                     ; don't block producers - discard stale values

(tests
  (def !x (atom 0))
  (with (e/run (tap (new (MyWatch !x))))                      ; Functions that return flows are "DAG constructors"
    % := 0
    (swap! !x inc)
    % := 1))

; `MyWatch` is a function that constructs a missionary flow (DAG) and returns it as a value.
; In Electric Clojure, use (new) to join such a flow value into the Electric Clojure execution context.
; This operation of flattening a flow-of-flows into the main flow is the same as "await" or "flatmap" aka "monadic join".


; m/observe is how you hook the Electric Clojure object lifecycle:

(defn ConstantObject "Returns a recipe for a constant signal that logs its lifecycle to provided callback"
  [x trace!]
  (->> (m/observe
         (fn constructor [send!]
           (trace! ::mount)
           ; signal emits initial value on construction, and then signal remains constant until termination
           (send! x)
           (fn destructor [] (trace! ::unmount))))
       (m/relieve {})))

(tests "object lifecycle"
  (def !x (atom 0))
  (def dispose
    (e/run
      (tap
        (let [x (e/watch !x)]
          (when (even? x)
            (new (ConstantObject x tap))))))) ; Electric Clojure will unmount/destruct the flow when the if switches back
  % := ::mount
  % := 0
  (swap! !x inc)
  % := ::unmount
  % := nil
  (swap! !x inc)
  % := ::mount
  % := 2
  (dispose)                                                 ; explicit dispose, to test final unmount
  % := ::unmount)
