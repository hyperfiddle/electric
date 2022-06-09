(ns wip.counter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom3 :as dom])
 (:import (hyperfiddle.photon Pending Remote)))

(defn inc! [!state] (swap! !state inc))

(p/defn Counter []
  (let [!state (atom 0)]
    (dom/hiccup
      [:div
       [:input {:value (p/watch !state)}]
       [:button "Count"
        (new (dom/events "click" (map (partial inc! !state))))]])))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/node (dom/by-id "root")]
                   (Counter.))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main))))