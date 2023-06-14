(ns peter.y2023.edef-glitch
  (:require [hyperfiddle.electric :as e]
            [missionary.core :as m]
            [contrib.trace :as ct]
            [contrib.trace.datascript-tracer :as ds-tracer]))

;; I don't understand why in 3 cases we see the 1 again

(def !x (atom 1))
(e/def x (e/client (e/watch !x)))
(e/defn EDefExample []
  (e/client
    ((m/sp (m/? (m/sleep 1000)) (swap! !x inc)) identity identity)
    (ct/trace :edef/direct (e/server x))             ; Pending -> 1 -> 2
    (ct/trace :edef/wrapped (e/server (e/client x))) ; Pending -> 1 -> Pending -> 1 -> 2
    ))

(e/defn LetExample []
  (e/client
    (let [!x (atom 1), x (e/watch !x)]
      ((m/sp (m/? (m/sleep 1000)) (swap! !x inc)) identity identity)
      (ct/trace :let/direct (e/server x))             ; Pending -> 1 -> Pending -> 1 -> 2
      (ct/trace :let/wrapped (e/server (e/client x))) ; Pending -> 1 -> Pending -> 1 -> 2
      )))

(e/defn Glitch []
  (e/client
    (ds-tracer/with-defaults
      (EDefExample.)
      (LetExample.)
      (ds-tracer/DatascriptTraceView.))))
