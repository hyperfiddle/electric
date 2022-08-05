(ns user.demo-6-two-clocks
  "network stress test"
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  (:import (hyperfiddle.photon Pending))
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
  (dom/div
    (dom/h1 (dom/text "Two Clocks"))

    (let [c (p/client (new (clock)))
          s (p/server (new (clock)))]

      (dom/p (dom/span (dom/text "client time: "))
             (dom/span (dom/text c)))

      (dom/p (dom/span (dom/text "server time: "))
             (dom/span (dom/text s)))

      (dom/p (dom/span (dom/text "latency: "))
             (dom/span (dom/text (- c s)))))))
