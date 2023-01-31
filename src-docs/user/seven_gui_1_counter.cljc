(ns user.seven-gui-1-counter
  #?(:cljs (:require-macros user.seven-gui-1-counter))
  (:require
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom2 :as dom]
    [hyperfiddle.photon-ui4 :as ui]))

;; https://eugenkiss.github.io/7guis/tasks#counter

(p/defn Counter []
  (p/client
    (let [!state (atom 0)]
      (dom/p (dom/text (p/watch !state)))
      (ui/button (p/fn [] (swap! !state inc))
        (dom/text "Count")))))
