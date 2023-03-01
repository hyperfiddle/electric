(ns user.demo-svg
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-svg :as svg]))

(defn wave [time]
  (Math/cos (/ (* (mod (Math/round (/ time 10)) 360) Math/PI) 180)))

(e/defn SVG []
  (e/client
    (let [offset (* 3 (wave e/system-time-ms))]
      (dom/h1 (dom/text "SVG Example"))
      (svg/svg (dom/props {:viewBox "0 0 300 100"})
        (svg/circle (dom/props {:cx    50
                                :cy    50
                                :r     (+ 30 offset)
                                :style {:fill "#af7ac5 "}}))
        (svg/g (dom/props {:transform (str "translate(105,20) rotate(" (* 3 offset) ")")})
          (svg/polygon (dom/props {:points "30,0 0,60 60,60"
                                   :style  {:fill "#5499c7"}})))
        (svg/rect (dom/props {:x      200
                              :y      20
                              :width  (+ 60 offset)
                              :height (+ 60 offset)
                              :style  {:fill "#45b39d"}}))))))
