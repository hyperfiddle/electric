(ns user.demo-controlled-input
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui]
   [missionary.core :as m])
  #?(:cljs (:require-macros user.demo-controlled-input)))

(def latency-ms 300)
(defn reset-slowly! [atm v]
  (p/task->cp (m/sp (m/? (m/sleep latency-ms)) (reset! atm v))))

(p/defn App []
  (p/client
    (dom/div
      (p/server
        (let [!x (atom 0) x (p/watch !x)]
          (p/client
            (when-some [v (ui/input x)] (p/server (new (reset-slowly! !x v))))
            (dom/div (dom/text "server value: " x))))))))
