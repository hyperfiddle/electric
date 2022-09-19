(ns user.missionary.missionary-advanced
  (:require [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

(tests
  "break a flow by sending a new value before previous was sampled"
  (def !x (atom 0))
  (def >x (m/observe (fn [send!]
                       (add-watch !x ::MyWatch (fn [k ref old new]
                                                 (send! new)))
                       (send! @!x)
                       ; because m/observe is about interop, it has a buffer of size 1
                       ; so that we can accept the initial value and notify that it is ready
                       (fn []
                         (remove-watch !x ::MyWatch)))))
  (def !it (>x #(tap :notify) #(tap :terminate)))
  % := :notify
  ;@!it := 1                                                 ; DON'T consume the value from m/observe buffer
  ; if you don't, the next event is undefined behavior. - Consumer is not ready
  ; the send! throws; because we have not sampled to consume the previous value
  (swap! !x inc) thrown? java.lang.Error
  (!it)

  ; m/ap also has a buffer,
  ; because we don't know at compile time the branches that will produce a value
  ; so now we have two buffers:
  (def !it ((m/ap (inc (m/?> >x))) #(tap :notify) #(tap :terminate)))
  % := :notify
  (swap! !x inc)
  (swap! !x inc) thrown? java.lang.Error)
