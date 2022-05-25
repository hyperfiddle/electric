(ns dustin.y2022.photon-dom-event-bubbling
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [hyperfiddle.zero :as z]))

; current way

(p/defn Click-counter []
  (dom/input
    (dom/attribute "type" "button")
    (dom/attribute "value" "Click me!")
    (->> (dom/events dom/parent dom/click-event)
         #_(m/reductions (fn [r event] (inc r)) 0)          ; no cc/fn in Photon yet
         (m/eduction (map (constantly 1)))
         (m/reductions + 0)
         (m/relieve {})
         new)))

(def !click-count (atom 0))

(p/defn Counting-component []
  (dom/div
    (dom/text "The atom ")
    (dom/code (dom/text "click-count"))
    (dom/text " has value: ")
    (dom/text (p/watch !click-count))
    (dom/text ". ")
    ; counter must render at this location, so cannot lift it into higher scope to access
    ; the count signal, therefore use state to "loop" by side effect in React style
    (->> (Click-counter.)
         (reset! !click-count))))

; Experimental better way (not implemented)
;  - event bubbling
;  - continuous time events

(p/defn Counting-component []
  (let [el (Click-counter. (p/watch !label))]
    (dom/div
      (dom/text "count: ")
      (dom/text (new (m/reductions + 0 (p/fn [] (el ::n)))))
      el)))

(p/defn Click-counter [label]
  (dom/div
    (dom/span (dom/text label))
    (dom/input (dom/attribute "type" "button"))
    ::n (if (z/impulse p/frame (dom/events dom/parent dom/input-event)) 1 0)))
