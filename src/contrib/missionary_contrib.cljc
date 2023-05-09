(ns contrib.missionary-contrib
  "staging area, to be considered for missionary inclusion?"
  (:require [clojure.core.async :as a]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]])
  (:import (missionary Cancelled)))

(defn mix [& flows] (m/ap (m/?> (m/?> (count flows) (m/seed flows)))))

(defn iterator-consumer "blocking iterable pattern"
  [^java.util.Iterator it]
  ; why not one thread tied to the iterator extent?
  ; (future (while (.hasNext it) (! (.next it))))
  (m/ap
    (loop []
      (if (m/? (m/via m/blk (.hasNext it)))
        (m/amb (m/? (m/via m/blk (.next it))) (recur))
        (m/amb)))))

(defn seq-consumer [xs] ; xs is iterable
  (m/ap
    (loop [xs xs]
      (if (m/? (m/via m/blk (seq xs)))
        (m/amb (m/? (m/via m/blk (first xs))) (recur (rest xs)))
        (m/amb)))))

#?(:clj
   (tests
     (def !it (.iterator (.keySet (java.lang.System/getProperties))))
     (->> (iterator-consumer !it)
          (m/eduction (take 3))
          (m/reduce conj []) m/?)
     := ["java.specification.version" "sun.jnu.encoding" "java.class.path"]

     ; careful, Java iterator is stateful

     (def xs (iterator-seq (.iterator (.keySet (java.lang.System/getProperties)))))
     (take 3 xs) := ["java.specification.version" "sun.jnu.encoding" "java.class.path"]

     (->> (seq-consumer xs)
          (m/eduction (take 3))
          (m/reduce conj []) m/?)
     := ["java.specification.version" "sun.jnu.encoding" "java.class.path"]))

(defn poll-task
  "derive discrete flow from succession of polled values from a task (or mbox)"
  [task]
  #_(m/ap (m/? (m/?> (m/seed (repeat mbox)))))
  (m/ap
    (loop [v (m/? task)]
      (m/amb v (recur (m/? task))))))

(defn document
  "compare (document log) to (d/entity db eid). if a datomic txn is [op eid a v], 
log here is [op a v], or in other words, there is only one entity (the `eid` is 
constant) so we are left with not an entity but a document."
  [>txs]
  (m/reductions (fn [m [op a v]] ; or is the a actually e?
                  (case op
                    ::add (assoc m a v)
                    ::retract (dissoc m a))) {} >txs))

;; Core.async interop

(defn chan-read!
  "Return a task taking one value from `chan`. Return nil if chan is closed. Does not close chan,
  and when cancelled stops waiting for chan."
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
