(ns user.seven-gui-1-counter
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-dom2 :as dom2]
   [hyperfiddle.photon-ui4 :as ui4])
  #?(:cljs (:require-macros user.seven-gui-1-counter)))

;; https://eugenkiss.github.io/7guis/tasks#counter

(p/defn Counter []
  (p/client
    (let [!state (atom 0)]
      (dom/p (p/watch !state))
      (ui4/button (p/fn [] (swap! !state inc))
        (dom2/text "Count")))))
