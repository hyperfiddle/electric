(ns user.demo-two-clocks
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

(e/defn TwoClocks []
  (e/client
    (dom/h1 (dom/text "Two Clocks — Electric Clojure"))

    (let [c (e/client e/system-time-ms)
          s (e/server e/system-time-ms)]

      (dom/div (dom/text "client time: " c))
      (dom/div (dom/text "server time: " s))
      (dom/div (dom/text "latency: " (- s c))))))
