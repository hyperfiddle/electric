(ns wip.counter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
 (:import (hyperfiddle.photon Pending Remote)))

(defn inc! [!state _js-event]
  (swap! !state inc))

(p/defn Counter []
  (let [!state (atom 0)]
    (dom/div
     (ui/input {:value (p/watch !state)})
     (ui/button {:on-click (map (partial inc! !state))}
                (dom/text "Count2")))))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/node (dom/by-id "root")]
                   (Counter.))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
