(ns user.seven-gui-1-counter
  #?(:cljs (:require-macros user.seven-gui-1-counter))
  (:require
    [hyperfiddle.electric :as p]
    [hyperfiddle.electric-dom2 :as dom]
    [hyperfiddle.electric-ui4 :as ui]))

;; https://eugenkiss.github.io/7guis/tasks#counter

(p/defn Counter []
  (p/client
    (let [!state (atom 0)]
      (dom/p (dom/text (p/watch !state)))
      (ui/button (p/fn [] (swap! !state inc))
        (dom/text "Count")))))
