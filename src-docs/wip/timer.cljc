(ns wip.timer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom3 :as dom]
            [wip.semicontroller])
  #?(:require-macros [wip.semicontroller :refer [interpreter]])
  (:import (hyperfiddle.photon Pending Remote)))

(def initial-goal 10000)

(defn seconds [milliseconds] (/ (Math/floor (/ milliseconds 100)) 10))

(defn now [] #?(:cljs (js/Date.now)))

(p/defn Timer []
  (let [!goal  (atom initial-goal)
        !start (atom (now))
        start  (p/watch !start)
        goal   (p/watch !goal)
        time   (min goal (- (new dom/clock 1) ; clock ticking at 1Hz
                            start))]
    (dom/hiccup
      [:div {:style {:display     :grid
                     :margin-left "20rem"
                     :grid-gap    "0 1rem"
                     :align-items :center}}
       [:span "Elapsed Time:"]
       [:progress {:max   goal
                   :value time
                   :style {:grid-column 2}}]
       [:span [:text (str (seconds time) " s")]]
       [:span {:style {:grid-row 3}} "Duration"]
       [:input {:type  :range
                :min   0
                :max   60000
                :value initial-goal
                :style {:grid-row 3}}
        (new (dom/events "input" (comp (map dom/target-value) (map (partial reset! !goal))) initial-goal))]
       [:button {:style {:grid-row 4, :grid-column "1/3"}}
        "Reset"
        (new (dom/events "click" (comp (map now) (map (partial reset! !start)))))]])))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/node (dom/by-id "root")]
                   (Timer.))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main))))