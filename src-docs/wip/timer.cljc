(ns wip.timer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom3 :as dom]
            [wip.semicontroller])
  #?(:require-macros [wip.semicontroller :refer [interpreter]])
  (:import (hyperfiddle.photon Pending Remote)))

(def initial-goal 10000)

(defn seconds [milliseconds] (/ (Math/floor (/ milliseconds 100)) 10))

(defn now [] (js/Date.now))

(p/defn Timer []
  (let [!goal  (atom initial-goal)
        !start (atom (now))
        start  (p/watch !start)
        goal   (p/watch !goal)
        time   (min goal (- (new dom/clock 1) ; clock ticking at 1Hz
                            start))]
    (dom/div (dom/style {:display     :grid
                         :margin-left "20rem"
                         :gap         "0 1rem"
                         :align-items :center})
      (dom/span (dom/text "Elapsed Time:"))
      (dom/progress (dom/props {:max   goal
                                :value time
                                :style {:grid-column 2}}))
      (dom/span (dom/text (str (seconds time) " s")))
      (dom/span (dom/style {:grid-row 3})
        (dom/text "Duration"))
      (dom/input (dom/props {:type  :range
                             :min   0
                             :max   60000
                             :value initial-goal
                             :style {:grid-row 3}})
        (new (dom/events "input" (comp (map dom/target-value) (map (partial reset! !goal))) initial-goal)))
      (dom/button (dom/style {:grid-row 4, :grid-column "1/3"})
        (dom/text "Reset")
        (new (dom/events "click" (comp (map now) (map (partial reset! !start)))))))))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/parent (dom/by-id "root")]
                   (Timer.))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main))))