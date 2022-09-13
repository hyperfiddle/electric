(ns user.missionary.missionary-tutorial-object
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

(defn ConstantObject
  "build a continuous flow that emits a constant on construction, and then remains constant until
  cancelled"
  [x trace!]
  (->> (m/observe
         (fn constructor [emit!]
           (trace! ::mount)
           (emit! x)
           (fn destructor [] (trace! ::unmount))))
       (m/latest identity)
       (m/relieve {})))

(tests "object lifecycle"
  (def !x (atom 0))
  (def <x (m/cp (let [x (m/?< (m/watch !x))]
                  (if (even? x)
                    ; object is destroyed when the if switches, and then recreated next time
                    (m/?< (ConstantObject x !))
                    ::off))))
  (def it (<x #(! ::notify) #(! ::terminate)))
  % := ::notify

  (! @it) ; tap the result to RCF queue so that we can test the order
  % := ::mount ; mount happens before 0 is produced
  % := 0 ; 0 was produced by sampling @it

  (swap! !x inc)
  % := ::notify

  (! @it)
  % := ::unmount
  % := ::off

  (swap! !x inc)
  % := ::notify

  (! @it)
  % := ::mount
  % := 2

  (it)
  % := ::notify

  @it thrown? missionary.Cancelled
  ; unmount and terminate happen BEFORE the cancelled, in response to @it
  ; but are tapped to RCF and we can only test the queue after.
  % := ::unmount
  % := ::terminate
  )