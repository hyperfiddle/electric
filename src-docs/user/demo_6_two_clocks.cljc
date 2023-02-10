(ns user.demo-6-two-clocks
  "network stress test"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros user.demo-6-two-clocks)))

(defn clock []
  (->> (m/ap
         (loop []
           (m/amb nil
             (do (m/? (m/sleep 10))
                 (recur)))))
    (m/sample (fn [_]
                #?(:clj (System/currentTimeMillis)
                   :cljs (js/Date.now))))))

(e/defn App []
  (e/client
    (dom/h1 (dom/text "Two Clocks"))

    (let [c (e/client (new (clock)))
          s (e/server (new (clock)))]

      (dom/div (dom/text "client time: " c))
      (dom/div (dom/text "server time: " s))
      (dom/div (dom/text "latency: " (- s c))))))
