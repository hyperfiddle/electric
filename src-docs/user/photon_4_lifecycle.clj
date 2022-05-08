(ns user.photon-4-lifecycle
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]]
            [missionary.core :as m]))


; Missionary flows, and transitively Photon flows, have an object lifecycle (mount/unmount in React terms).
; Reactive computations have a "boot" phase (the expensive initial compute). Once booted, flows are incrementally
; maintained (a small change to an input results in a small adjustment to the output). Eventually, the program will
; terminate gracefully, terminate with error (crash), or be cancelled from the outside.

; Lifecycle hello world is implementing m/watch, which subscribes to an atom:

(defn MyWatch "a watch is a signal derived from observing changes to an atom"
  [!x]
  (->> (m/observe                                           ; missionary primitive for bridging a ref
         (fn constructor! [send!]                           ; mount
           ; in ctor, setup the subscriptions to the atom
           (add-watch !x ::MyWatch (fn [k ref old new]
                                     ; bridge each atom change event to the signal
                                     (send! new)))
           (send! @!x)                                      ; bridge the initial value during construction
           (fn destructor! []                               ; unmount
             (remove-watch !x ::MyWatch))))                 ; unsubscribe and release resources
       (m/relieve {})))                                     ; discard stale values

(tests
  (def !x (atom 0))
  (with (p/run (! (new (MyWatch !x))))                      ; functions are DAG constructors
    % := 0
    (swap! !x inc)
    % := 1))

; `MyWatch` is a function that constructs a missionary flow (DAG) and returns it as a value.
; In Photon, use (new) to join such a flow value into the Photon execution context.
; This operation of flattening a flow-of-flows into the main flow is the same as "await" or "flatmap" aka "monadic join".


; m/observe is how you hook the Photon object lifecycle:

(defn Constant "Returns a recipe for a constant flow that logs its lifecycle to provided callback"
  [x trace!]
  (m/observe (fn [send!]
               (trace! ::mount)
               ; this will only ever send one value on construction
               (send! x)
               ; and then wait until termination
               #(trace! ::unmount))))

(tests "object lifecycle"
  (def !x (atom 0))
  (def dispose
    (p/run
      (!
        (let [x (p/Watch. !x)]
          (if (even? x)
            (new (Constant x !)))))))                       ; Photon will unmount/destruct the flow when the if switches back
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
