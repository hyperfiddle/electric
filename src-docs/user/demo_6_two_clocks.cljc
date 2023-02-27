(ns user.demo-6-two-clocks
  "network stress test"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

(e/defn App []
  (e/client
    (dom/h1 (dom/text "Two Clocks"))

    (let [c (e/client e/system-time-ms)
          s (e/server e/system-time-ms)]

      (dom/div (dom/text "client time: " c))
      (dom/div (dom/text "server time: " s))
      (dom/div (dom/text "latency: " (- s c))))))
