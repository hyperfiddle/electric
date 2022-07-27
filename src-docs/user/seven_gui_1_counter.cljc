(ns user.seven-gui-1-counter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  (:import (hyperfiddle.photon Pending)))

;; https://eugenkiss.github.io/7guis/tasks#counter

(p/defn Counter []
  (let [!state (atom 0)]
    (dom/div
      (dom/p (dom/text (p/watch !state)))
      (ui/button {::ui/click-event (p/fn [_] (swap! !state inc) nil)}
        (dom/text "Count")))))

(def main
  #?(:cljs (p/boot
             (try
               (binding [dom/node (dom/by-id "root")]
                 (Counter.))
               (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )
