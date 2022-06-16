(ns dustin.rec
  (:require [hfdl.lang :as r :refer []]
            [hyperfiddle.photon-dom :as d]
            [missionary.core :as m]))

(r/def bmi 30)
(r/def weight 10)

(r/defn slider [fv min max]
  (d/input {"type"  "range"
            "value" (f bmi)
            "min"   min
            "max"   max
            :style  {:width "100%"}}
           (d/events "input" (map (dom/oget :target :value)))))

(r/defn bmi-component []
  (let [height (r/$ slider 170 100 220)]
    (r/binding [weight (r/$ slider (r/fn [bmi] (* bmi height height)) 30 150)
                bmi (r/$ slider #'(/ weight (* height height)) 10 50)]
      bmi)))

(def ^:dynamic bmi)
(def ^:dynamic weight)

(defn slider' [f min max]
  (println (f bmi weight)))

(defn bmi-component' []
  (let [height (slider 170 100 220)]
    (binding [weight (slider (fn [bmi weight] (m/cp 30)) 30 150)
              bmi (slider (fn [bmi weight] (m/cp 10)) 10 50)]

      (binding [weight (slider (fn [bmi weight] (* bmi height height)) 30 150)
                bmi (slider (fn [bmi weight] (/ weight (* height height))) 10 50)]

        bmi))))





(r/defn bmi-component []
  (let [height (r/$ slider 170 100 220)]
    (r/rec [weight (r/$ slider #'(* bmi height height) 30 150)
            bmi (r/$ slider #'(/ weight (* height height)) 10 50)]
      bmi)))