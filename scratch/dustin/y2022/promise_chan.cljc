(ns dustin.y2022.scratch
  (:require [missionary.core :as m]
            [clojure.core.async :as a]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  (:import [hyperfiddle.photon Pending Failure]
           (missionary Cancelled)))

(hyperfiddle.rcf/enable!)

(defn chan-read
  "Return a task taking a value from `chan`. Retrun nil if chan is closed. Does
   not close chan, and stop reading from it when cancelled."
  [chan]
  (fn [success failure]                                     ; a task is a 2-args function, success and failure are callbacks.
    (let [cancel-chan (a/chan)]                             ; we will put a value on this chan to cancel reading from `chan`
      (a/go (let [[v port] (a/alts! [chan cancel-chan])]    ; race between two chans
              (if (= port cancel-chan)                      ; if the winning chan is the cancelation one, then task has been cancelled
                (failure (Cancelled.))                      ; task has been cancelled, must produce a failure state
                (success v)                                 ; complete task with value from chan
                )))
      ;; if this task is cancelled by its parent process, close the cancel-chan
      ;; which will make cancel-chan produce `nil` and cause cancellation of read on `chan`.
      #(a/close! cancel-chan))))

(defn chan->flow
  "Produces a discreet flow from a core.async `channel`"
  [channel]
  (m/ap                                                     ; returns a discreet flow
    (loop []
      (if-some [x (m/? (chan-read channel))]                ; read one value from `channel`, waiting until `channel` produces it
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

#?(:clj
   (tests
     "Turn a channel into a discreet flow"
     (let [c (a/chan)
           f (p/chan->flow c)
           it (f #(tap :ready) #(tap :done))]
       (a/put! c 1)
       ;; chan-read rely on a go block, which will run its body in another thread.
       ;; We can not assume flow is immediately ready. Hence we await for :ready with %
       % := :ready
       @it := 1)))
