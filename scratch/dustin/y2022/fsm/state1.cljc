(ns dustin.y2022.fsm.state1
  (:require
    [hyperfiddle.photon :as p]))


; stream of user interactions -> stream of commands -> handle commandstream of view states

; event -> command -> reducer -> value

; reduction

(p/defn transition [s]
  (case )
        )


(p/defn Reject [e] ::rejected)
(p/defn Approve [e] ::approve)

(p/defn Payment [record]
  (NewPayment. record))

(defn init [record] (assoc record :status :new))

; states
:new :rejected :approved :paid

; commands
(defn reject [state])
(defn approve [state] (throw (Pending.)) (throw (Failure.)) :approved)
(defn pay [state])

; transitions
[[:new #{reject approve}]
 [:approved #{pay}]
 [:paid #{}]

 [:pending]
 [:error]]

; reducer encoding
(defn transition [state command]
  (case state
    :new (case command
           :reject (do (reject state) :rejected)
           :approve (do (approve state) :approved))
    :rejected (assert false)
    :approved (case command
                :pay (pay state) :paid)
    :paid (assert false)
    (assert false)))

;(reduce transition :new [:approve :pay])