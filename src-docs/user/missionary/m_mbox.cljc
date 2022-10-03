(ns user.missionary.m-mbox
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(tests
  "mbox"
  (def !m (m/mbx)) ; like a mailbox

  "post val into mbox" ; or "send" like agent
  (!m 1) ; mail man puts the flag up when he posts mail

  "await mbox as task to fetch the value"
  (m/? !m) := 1) ; careful at repl, m/? can lock

(defn mbx->flow [mbx]
  (m/ap
    (loop [v (m/? mbx)]
      (m/amb v (recur (m/? mbx))))))

(tests
  "mbox is a FIFO queue with unbounded buffer" ; it's not an atom, closer to agent
  (def !m (m/mbx))
  (doseq [x (range 10)]
    (!m x))

  (->> (mbx->flow !m)
       (m/eduction (take 5)) ; terminate after 5
       (m/reduce conj []) m/?)
  := [0 1 2 3 4]

  ; note the mbox flow never terminates as someone can post another value
  )

; an actor is a process with a mailbox
; process = stateful, effects, lifecycle
; an agent =