(ns user.tutorial-7guis-4-timer
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]))

;; https://eugenkiss.github.io/7guis/tasks#timer

(def initial-goal 10)                ; s

(defn seconds [milliseconds] (/ (Math/floor (/ milliseconds 100)) 10))

(defn second-precision [milliseconds] (-> milliseconds (/ 1000) (Math/floor) (* 1000))) ; drop milliseconds

(defn now [] #?(:cljs (second-precision (js/Date.now))))

(e/defn Timer []
  (e/client
    (dom/h1 (dom/text "7 GUIs: Timer"))
    (let [!goal (atom initial-goal)
          !start (atom (now))
          goal (e/watch !goal)
          goal-ms (* 1000 goal)
          start (e/watch !start)
          time (min goal-ms (- (second-precision e/system-time-ms) start))]
      (dom/div (dom/props {:style {:display :grid
                                        ;:margin-left "20rem"
                                    :width "20em"
                                    :grid-gap "0 1rem"
                                    :align-items :center}})
        (dom/span (dom/text "Elapsed Time:"))
        (dom/progress (dom/props {:max goal-ms, :value time, :style {:grid-column 2}}))
        (dom/span (dom/text (seconds time) " s"))
        (dom/span (dom/props {:style {:grid-row 3}}) (dom/text "Duration: " goal "s"))
        (ui4/range goal (e/fn [v] (reset! !goal v))
          (dom/props {:min 0, :max 60, :style {:grid-row 3}}))
        (ui4/button (e/fn [] (reset! !start (now)))
          (dom/props {:style {:grid-row 4, :grid-column "1/3"}})
          (dom/text "Reset"))))))
