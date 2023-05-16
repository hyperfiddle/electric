(ns contrib.missionary-contrib-test
  (:require [clojure.core.async :as a]
            [contrib.missionary-core-async :as mc]
            [hyperfiddle.electric :as p]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]] 
            [missionary.core :as m])
  (:import missionary.Cancelled))

;; TODO port to standard lib
(defn chan-write
  "Return a task writing `val` onto `chan`. Produces true if
   val was succesfully written, false if chan was closed."
  [chan val]
  (fn [success failure]
    (let [cancel-chan (a/chan)]
      (a/go (let [[v port] (a/alts! [[chan val] cancel-chan])]
              (if (= port cancel-chan)
                (failure (Cancelled.))
                (success v))))
      #(a/close! cancel-chan))))

;; TODO port to standard lib
(defn onto-chan
  "Transfer values from flow to chan. Produces a flow of values that couldn't
   be transfered onto chan."
  [flow chan]
  (m/ap (let [val (m/?> flow)] ; for each successive values of `flow`
          (if-not (m/? (chan-write chan val)) ; try to write it onto chan
            val ; if write failed, emit val
            (m/amb) ; emit nothing
            ))))

;; These tests are CLJ only. These tests could be made to pass in JS but added
;; value is low as constraints are external to the domain. m/? is not defined
;; outside of sp/ap/cp in JS and we rely on future and Thread/sleep. 
#?(:clj
   (tests
     "Read a value from a channel"
     (let [c (a/chan)
           t (mc/chan-read! c)]
       (a/put! c 1)
       (m/? t) := 1)))

#?(:clj
   (tests
     "Reading a value from a channel blocks until a value is available."
     (let [c (a/chan)
           t (mc/chan-read! c)]
       (future (tap (m/? t))) ; don't block main (repl) thread
       (a/put! c 1)
       % := 1)))

#?(:clj
   (tests
     "Write a value to a channel"
     (let [c (a/chan)
           t (chan-write c 1)]
       (a/take! c tap)
       (m/? t) := true
       % := 1)))

#?(:clj
   (tests
     "Writing a value to a channel blocks if the channel is full."
     (let [c (a/chan)
           t (chan-write c 2)]
       (a/put! c 1) := true
       (Thread/sleep 100)
       (future (m/? t) := true)
       (Thread/sleep 100)
       (a/take! c tap)
       (a/take! c tap)
       % := 1
       % := 2)))

#?(:clj
   (tests
     "Turn a channel into a discrete flow"
     (let [c (a/chan)
           f (mc/chan->ap c)
           it (f #(tap :ready) #(tap :done))]
       (a/put! c 1)
       ;; chan-read rely on a go block, which will run its body in another thread.
       ;; We can not assume flow is immediately ready. Hence we await for :ready with %
       % := :ready
       @it := 1)))

#?(:clj
   (tests
     "Put values of a flow onto a channel, and read it back as a flow."
     (def c (a/chan))
     (future (tap (m/? (m/reduce conj ; just run the flow until it terminates
                               (m/ap (m/amb= (m/?> (onto-chan (m/seed [1 2]) c))
                                             (m/?> (mc/chan->ap c))))))))
     (a/close! c)
     % := [1 2]))

#?(:clj
   (tests
     "When transferring values from a discrete flow to a channel, values are not lost if the channel is closed."
     (def input (a/chan))
     (def c (a/chan))
     (future (m/? (m/reduce {} nil ; just run the flow until it terminates
                            (m/ap (m/amb= (tap [:success (m/?> (mc/chan->ap c))])
                                          (tap [:failure (m/?> (onto-chan (mc/chan->ap input) c))]))))))
     (a/>!! input 1) ; put 1 on input, which transfers to a flow, then is put onto c.
     (Thread/sleep 100) ; even if >!! is blocking, go blocks might race with the next instruction
     (a/close! c)
     (a/>!! input 2) ; put 2 on input, which will be put onto c, but c is closed.
     % := [:success 1] ; c got 1
     % := [:failure 2] ; onto-chan failed to write 2 on c and returned it.
     ))

#?(:clj
   (tests
     "Using a core.async channel from Electric"
     (def c (a/to-chan [1 2 3]))
     (with (p/run (tap (mc/use-channel c)))
           % := nil
           % := 1
           % := 2
           % := 3)))

;; TODO WIP
(comment
  #?(:clj
     (tests
      "Putting values on a channel from Electric"
      (def !a (atom 0))
      (def c (a/chan))
      (with (p/run (tap (new (onto-chan (p/fn [] (p/watch !a)) c))))
            (a/go-loop [x (a/<! c)]
              (when x
                (tap x)
                (recur (a/<! c))))
            ;; (swap! !a inc)
            ;; (a/close! c)
            ;; (swap! !a inc)
            % := 0
            % := 1
            % := 2
            ))))
