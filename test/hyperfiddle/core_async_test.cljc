(ns hyperfiddle.core-async-test
  "Photon language unit tests"
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [clojure.core.async :as a]
            [missionary.core :as m])
  (:import missionary.Cancelled
           hyperfiddle.photon.Pending))

(defn chan-read
  "Return a task taking a value from `chan`. Retrun nil if chan is closed. Does
   not close chan, and stop reading from it when cancelled."
  [chan]
  (fn [success failure] ; a task is a 2-args function, success and failure are callbacks.
    (let [cancel-chan (a/chan)] ; we will put a value on this chan to cancel reading from `chan`
      (a/go (let [[v port] (a/alts! [chan cancel-chan])] ; race between two chans
              (if (= port cancel-chan) ; if the winning chan is the cancelation one, then task has been cancelled
                (failure (Cancelled.)) ; task has been cancelled, must produce a failure state
                (success v) ; complete task with value from chan
                )))
      ;; if this task is cancelled by its parent process, close the cancel-chan
      ;; which will make cancel-chan produce `nil` and cause cancellation of read on `chan`.
      #(a/close! cancel-chan))))

(defn chan->flow
  "Produces a discreet flow from a core.async `channel`"
  [channel]
  (m/ap ; returns a discreet flow
    (loop []
      (if-some [x (m/? (chan-read channel))] ; read one value from `channel`, waiting until `channel` produces it
        ;; We succesfully read a non-nil value, we use `m/amb` with two
        ;; branches. m/amb will fork the current process (ap) and do two things
        ;; sequencially, in two branches:
        ;; - return x, meaning `loop` ends and return x, ap will produce x
        ;; - recur to read the next value from chan
        (m/amb x (recur))
        ;; `channel` producing `nil` means it's been closed. We want to
        ;; terminate this flow without producing any value (not even nil), we
        ;; use (m/amb) which produces nothing and terminates immediately. The
        ;; parent m/ap block has nothing to produce anymore and will also
        ;; terminate.
        (m/amb)))))

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
           t (chan-read c)]
       (a/put! c 1)
       (m/? t) := 1)))

#?(:clj
   (tests
     "Reading a value from a channel blocks until a value is available."
     (let [c (a/chan)
           t (chan-read c)]
       (future (! (m/? t))) ; don't block main (repl) thread
       (a/put! c 1)
       % := 1)))

#?(:clj
   (tests
     "Write a value to a channel"
     (let [c (a/chan)
           t (chan-write c 1)]
       (a/take! c !)
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
       (a/take! c !)
       (a/take! c !)
       % := 1
       % := 2)))

#?(:clj
   (tests
     "Turn a channel into a discreet flow"
     (let [c (a/chan)
           f (chan->flow c)
           it (f #(! :ready) #(! :done))]
       (a/put! c 1)
       ;; chan-read rely on a go block, which will run its body in another thread.
       ;; We can not assume flow is immediately ready. Hence we await for :ready with %
       % := :ready
       @it := 1)))

#?(:clj
   (tests
     "Put values of a flow onto a channel, and read it back as a flow."
     (def c (a/chan))
     (future (! (m/? (m/reduce conj ; just run the flow until it terminates
                               (m/ap (m/amb= (m/?> (onto-chan (m/seed [1 2]) c))
                                             (m/?> (chan->flow c))))))))
     (a/close! c)
     % := [1 2]))

#?(:clj
   (tests
     "When transfering values from a discreet flow to a channel, values are not lost if the channel is closed."
     (def input (a/chan))
     (def c (a/chan))
     (future (m/? (m/reduce {} nil ; just run the flow until it terminates
                            (m/ap (m/amb= (! [:success (m/?> (chan->flow c))])
                                          (! [:failure (m/?> (onto-chan (chan->flow input) c))]))))))
     (a/>!! input 1) ; put 1 on input, which transfers to a flow, then is put onto c.
     (Thread/sleep 100) ; even if >!! is blocking, go blocks might race with the next instruction
     (a/close! c)
     (a/>!! input 2) ; put 2 on input, which will be put onto c, but c is closed.
     % := [:success 1] ; c got 1
     % := [:failure 2] ; onto-chan failed to write 2 on c and returned it.
     ))

#?(:clj
   (tests
     "Using a core.async channel from Photon"
     (def c (a/to-chan [1 2 3]))
     (with (p/run (! (new (m/reductions {} nil (chan->flow c)))))
           % := nil
           % := 1
           % := 2
           % := 3)))