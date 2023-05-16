(ns contrib.missionary-core-async
  "Missionary adapters for core.async. Isolated so as to not create a hard 
library dependency on core.async."
  (:import (missionary Cancelled))
  (:require [clojure.core.async :as a]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]))

(defn chan-read!
  "Return a task taking one value from `chan`. Return nil if chan is closed. Does 
not close chan. Stops waiting for chan when cancelled."
  ([chan] (chan-read! chan (Cancelled.)))
  ([chan cancelled-value]
   (fn [success failure] ; a task is a 2-args function, success and failure are callbacks.
     (let [cancel-chan (a/chan)] ; we will put a value on this chan to cancel reading from `chan`
       (a/go (let [[v port] (a/alts! [chan cancel-chan])] ; race two chans
               (if (= port cancel-chan) ; if the winning chan is the cancelation one, then task has been cancelled
                 (failure cancelled-value) ; task has been cancelled, must produce a failure state
                 (success v)))) ; complete task with value from chan
       (fn cancel []
         ;; if this task is cancelled by its parent process, close the cancel-chan
         ;; which will make cancel-chan produce `nil` and cause cancellation of read on `chan`.
         (a/close! cancel-chan))))))

(defn chan->ap
  "Adapt a core.async channel to a discrete flow"
  [ch]
  (m/ap
    (loop []
      (if-some [x (m/? (chan-read! ch))] ; wait for one value, nil means channel closed.
        ;; We successfully read a non-nil value, we use `m/amb` with two branches. m/amb will fork
        ;; the current process (ap) and do two things sequentially, in two branches:
        ;; - return x, meaning `loop` ends and return x, ap will produce x
        ;; - recur to read the next value from chan
        (m/amb x (recur))
        ;; nil means the channel has been closed, so terminate this flow without producing any value
        ;; (not even nil). We use (m/amb) which produces nothing and terminates immediately. The
        ;; parent m/ap block has nothing to produce anymore and will also terminate.
        (m/amb)))))

(defmacro use-channel ; TODO rename
  ([chan] `(use-channel nil ~chan))
  ([init chan] `(new (m/reductions {} ~init (chan->ap ~chan)))))

(defn chan->task [ch]
  ; for streaming database results into a vector at the repl (which is not great)
  (->> (chan->ap ch)
    (m/reduce into [])))

;(defn chan->cp [ch] (->> (chan->ap ch) (m/reductions into []))) ; is this useful? Channels are discrete
