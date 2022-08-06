(ns user.seven-gui-4-timer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:cljs (:require-macros user.seven-gui-4-timer)))

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
        time   (min goal (- (second-precision (new dom/Clock 1)) ; clock ticking at 1Hz
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
             (ui/input {::ui/value       (/ initial-goal 1000)
                        ::ui/input-event (p/fn [event] (reset! !goal (* 1000 (js/parseInt (dom/oget event :target :value)))) nil)
                        ::dom/type       :range
                        ::dom/min        0
                        ::dom/max        60
                        ::dom/style      {:grid-row 3}})
             (ui/button {::dom/style      {:grid-row 4, :grid-column "1/3"}
                         ::ui/click-event (p/fn [_] (reset! !start (now)))}
                        (dom/text "Reset")))))
