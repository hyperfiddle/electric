(ns user.missionary.missionary-tutorial-watch
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

; Missionary flows, and transitively Photon flows, have an object lifecycle (mount/unmount in React terms).
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
  (def <x (m/cp (str (m/?< (MyWatch !x)))))
  (def it (<x #() #()))
  @it := "0"
  (swap! !x inc)
  @it := "1"
  (it))
