(ns user.demo-6-two-clocks
  "network stress test"
  (:require [hyperfiddle.electric :as p]
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
                   #?(:clj  (System/currentTimeMillis)
                      :cljs (js/Date.now))))))

(p/defn App []
  (p/client
    (dom/h1 (dom/text "Two Clocks"))

    (let [c (p/client (new (clock)))
          s (p/server (new (clock)))]

      (dom/div (dom/text "client time: " c))
      (dom/div (dom/text "server time: " s))
      (dom/div (dom/text "latency: " (- s c))))))
