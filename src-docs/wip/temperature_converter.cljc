(ns wip.temperature-converter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [clojure.string :as str])
  (:import (hyperfiddle.photon Pending)
           ))

(defn celsius->farenheit [c] (+ (* c (/ 9 5)) 32))
(defn farenheit->celsius [f] (* (- f 32) (/ 5 9)))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/node (dom/by-id "root")]
                   (let [!state      (atom 0)
                         temperature (p/watch !state)]
                     (dom/div
                      (dom/h1 (dom/text "Temperature Converter"))
                      (dom/dl
                       (dom/dt (dom/text "Celcius"))
                       (dom/dd (ui/numeric-input {:value     temperature
                                                  :step      "0.5"
                                                  :format    "%.2f"
                                                  :on-change (p/fn [value] (reset! !state value) nil)}))
                       (dom/dt (dom/text "Farenheit"))
                       (dom/dd (ui/numeric-input {:value     (celsius->farenheit temperature)
                                                  :step      "0.5"
                                                  :on-change (p/fn [value] (reset! !state (farenheit->celsius value)) nil)}))))))
                 (catch Pending _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main)))
  )
