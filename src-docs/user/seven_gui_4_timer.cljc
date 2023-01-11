(ns user.seven-gui-4-timer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-dom2 :as dom2]
            [hyperfiddle.photon-ui4 :as ui4])
  #?(:cljs (:require-macros user.seven-gui-4-timer)))

;; https://eugenkiss.github.io/7guis/tasks#timer

(def initial-goal 10)                ; s

(defn seconds [milliseconds] (/ (Math/floor (/ milliseconds 100)) 10))

(defn second-precision [milliseconds] (-> milliseconds (/ 1000) (Math/floor) (* 1000))) ; drop milliseconds

(defn now [] #?(:cljs (second-precision (js/Date.now))))

(p/defn Timer []
  (p/client
    (dom/h1 "7 GUIs: Timer")
    (let [!goal (atom initial-goal)
          !start (atom (now))
          goal (p/watch !goal)
          goal-ms (* 1000 goal)
          start (p/watch !start)
          time (min goal-ms (- (second-precision dom/system-time-ms)
                              start))]
      (dom/div {:style {:display :grid
                        ;:margin-left "20rem"
                        :width "20em"
                        :grid-gap "0 1rem"
                        :align-items :center}}
        (dom/span "Elapsed Time:")
        (dom/progress {:max goal-ms
                       :value time
                       :style {:grid-column 2}})
        (dom/span (seconds time) " s")
        (dom/span {:style {:grid-row 3}} "Duration: " goal "s")
        (ui4/range goal (p/fn [v] (reset! !goal v))
          (dom2/props {:min 0, :max 60, :style {:grid-row 3}}))
        (ui4/button (p/fn [] (reset! !start (now)))
          (dom2/props {:style {:grid-row 4, :grid-column "1/3"}})
          (dom2/text "Reset"))))))
