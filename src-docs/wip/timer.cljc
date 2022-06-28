(ns wip.timer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:require-macros [wip.timer])
  (:import (hyperfiddle.photon Pending)))

;; https://eugenkiss.github.io/7guis/tasks#timer

(def initial-goal 10000)                ; ms

(defn seconds [milliseconds] (/ (Math/floor (/ milliseconds 100)) 10))

(defn second-precision [milliseconds] (-> milliseconds (/ 1000) (Math/floor) (* 1000))) ; drop milliseconds

(defn now [] #?(:cljs (second-precision (js/Date.now))))

(p/defn Timer []
  (let [!goal  (atom initial-goal)
        !start (atom (now))
        goal   (p/watch !goal)
        start  (p/watch !start)
        time   (min goal (- (second-precision (new dom/clock 1)) ; clock ticking at 1Hz
                            start))]
    (dom/div {:style {:display     :grid
                      :margin-left "20rem"
                      :grid-gap    "0 1rem"
                      :align-items :center}}
             (dom/span (dom/text "Elapsed Time:"))
             (dom/progress {:max   goal
                            :value time
                            :style {:grid-column 2}})
             (dom/span (dom/text (str (seconds time) " s")))
             (dom/span {:style {:grid-row 3}} (dom/text "Duration"))
             (ui/input {:type     :range
                        :min      0
                        :max      60
                        :value    (/ initial-goal 1000)
                        :style    {:grid-row 3}
                        :on-input (p/fn [event] (reset! !goal (* 1000 (js/parseInt (dom/oget event :target :value)))) nil)})
             (ui/button {:style    {:grid-row 4, :grid-column "1/3"}
                         :on-click (p/fn [_] (reset! !start (now)))}
                        (dom/text "Reset")))))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/node (dom/by-id "root")]
                   (Timer.))
                 (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
