∏(ns dustin.y2022.photon-dom-event-bubbling
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [hyperfiddle.rcf :as rcf :refer [tests]]))

; current way

(p/defn Click-counter []
  (dom/input {:type "button"
              :value "Click me!"}
     (dom/events "click" (map (constantly 1)) 0 +) ; no cc/fn in Photon yet
     ))

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
    (dom/input {:type "button"})
    ::>n (dom/events "input")))

; next day

(tests
  ((constantly 1) 1 2) := 1
  (first 1 2) := 1
  (inc 1 2)
  ((comp inc) 1 2)
  ((comp inc) 1)
  (identity 1 2)
  (defn id [x & _] x)
  (defn inc' [x & _] (inc x))
  (def id' #(do %& %1))

  ((comp inc) 1 0 0)
  ((comp inc id) 1 0 0) := 2
  ((comp inc id') 1 0 0) := 2
  ((comp inc #(do %& %1)) 1 0 0) := 2
  ((comp inc #(nth %& 0)) 1 0 0) := 2
  )

(defn inc-rf [acc _] (inc acc))

(p/defn Counting-component []
  (let [{:keys [::<n] :as el} (Click-counter. (p/watch !label))]
    (dom/div
      (dom/text "count: ")
      (dom/text (new (m/reductions inc-rf 0 <n)))
      el)))

(p/defn Click-counter [label]
  (dom/div
    (dom/span (dom/text label))
    (dom/input {:type "button"})
    ::n (if (p/impulse p/frame (dom/>events "input")) 1 0)))
