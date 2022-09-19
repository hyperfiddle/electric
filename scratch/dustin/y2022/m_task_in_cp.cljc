(ns dustin.y2022.m-task-in-cp
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

(comment
  "Thought experiment: can m/amb and m/? be defined sensibly in m/cp?"
  (def !x (atom 0))
  (def <y (m/cp (m/amb 0 (m/? (m/sleep (m/?< (m/watch !x)) ::x)))))
  (def !it (<y #(tap ::notify) #(tap ::terminate)))
  % := ::notify
  @!it := 0
  ;% := ::notify -- sleep cannot begin until sampled, nothing to notify
  ;@!it := ::x
  )
