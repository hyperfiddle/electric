(ns wip.missionary-clock
  (:require [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests ! % with]]))


(hyperfiddle.rcf/enable!)

(defn clock []
  (->> (m/ap (loop [] (m/amb (m/? (m/sleep 1 1)) (recur))))
       (m/reductions + 0)                                   ; logical clock
       #_(m/reductions {} ::tick)                           ; physical clock
       #_(m/relieve {})                                     ; heater, running clock at max speed, no laziness
       ))

(tests
  (def >clock (clock))
  (def !it (>clock (fn [] (! ::notify))
                   (fn [] (! ::terminate))))
  % := ::notify
  @!it := 0
  % := ::notify
  @!it := 1
  % := ::notify
  (!it))
