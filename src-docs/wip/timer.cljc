(ns wip.timer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:require-macros [wip.timer])
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
       (ui/input {:type  :range
                  :min   0
                  :max   60000
                  :value initial-goal
                  :style {:grid-row 3}
                  :on-input (comp (map (dom/oget :target :value)) (map (partial reset! !goal)))})
       (let [on-off (ui/checkbox {:checked true})]
         (dom/text "on?" on-off))
       (ui/button {:style {:grid-row 4, :grid-column "1/3"}
                   :on-click (comp (map now) (map (partial reset! !start)))}
                  (dom/text "Reset"))])))

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
