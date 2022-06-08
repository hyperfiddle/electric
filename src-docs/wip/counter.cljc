(ns wip.counter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom3 :as dom])
 (:import (hyperfiddle.photon Pending Remote)))

(defn inc! [!state] (swap! !state inc))

(p/defn Counter []
  (let [!state (atom 0)]
    (dom/div
      (dom/input (dom/props {:value (p/watch !state)}))
      (dom/button (dom/text "Count")
        (new (dom/events "click" (map (partial inc! !state))))))))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/parent (dom/by-id "root")]
                   (Counter.))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main))))