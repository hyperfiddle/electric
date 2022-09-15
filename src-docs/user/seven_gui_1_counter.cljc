(ns user.seven-gui-1-counter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:cljs (:require-macros user.seven-gui-1-counter)))

;; https://eugenkiss.github.io/7guis/tasks#counter

(p/defn Counter []
  (p/client
    (let [!state (atom 0)]
      (dom/p (p/watch !state))
      (ui/button {::ui/click-event (p/fn [_] (swap! !state inc) nil)}
        "Count"))))
