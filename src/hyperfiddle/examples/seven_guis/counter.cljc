(ns hyperfiddle.examples.seven-guis.counter
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.examples.seven-guis.counter :refer [Counter counter]])))

(defn inc-reducer [r _] (inc r))

(photon/defn Counter [c]
  (dom/div
   (dom/input (dom/style {"margin-right" "1rem"})
              (dom/attribute "value" c)
              (dom/attribute "disabled" true))
   (dom/button
    (dom/text "Count")
    ~(->> (dom/events dom/parent dom/click-event)
          (m/eduction (map (constantly 1)))
          (m/relieve +)
          (m/reductions +)))))

(photon/def counter
  #'~@(let [>count! (z/state 0)
            v       ~>count!]
        (>count! ~@ (photon/$ Counter v))))
