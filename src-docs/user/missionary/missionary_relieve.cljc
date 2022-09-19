(ns user.missionary.missionary-relieve
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

; m/relieve is a dangerous primitive, yes it converts a discrete flow into a continuous flow BUT it does so by consuming
; the upstream flow as fast as possible and reducing it into an accumulator. In other words it "relieves" backpressure.
;
; This is appropriate for event sources like user interactions (typing into a dom input) because you only care about
; the freshest value and you never want your backpressure to slow down the user from typing.
;
; HOWEVER, by disabling backpressure you've ... lost all the benefits of backpressure, the whole point of backpressure
; is to tell the producer to slow down, backpressure is the right default.
;
; the dangerous part is that m/relieve can be used to create "heaters" like this monster, which will turn your fan on while counting as fast as it can:

(tests
  (def <spinner
    (->> (m/ap (loop [] (m/amb (m/? (m/sleep 0 1)) (recur))))
         (m/reductions + 0)
         (m/relieve {})))
  (def !it (<spinner #(tap ::notify) #(tap ::terminate)))
  % := ::notify
  @!it := 1
  % := ::notify
  @!it := 2
  % := ::notify
  @!it := 3
  (!it))

;

(tests
  "This counter is backpressured, it will count as fast as the consumer samples"
  (def <counter
    (->> (m/ap (loop [] (m/amb (m/? (m/sleep 0 1)) (recur))))
         (m/reductions + 0)
         (m/latest identity)))
  (def !it (<counter #(tap ::notify) #(tap ::terminate)))
  % := ::notify
  @!it := 1
  % := ::notify
  @!it := 2
  % := ::notify
  @!it := 3
  (!it))

; In all cases we use m/reductions to add an initial value because continuous flows must always be defined.
; Discrete flows typically will have a delay before they emit their first event

