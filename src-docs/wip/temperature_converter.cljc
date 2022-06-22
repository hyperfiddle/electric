(ns wip.temperature-converter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui2 :as ui]
            [clojure.string :as str])
  (:import (hyperfiddle.photon Pending Remote)))

(defn celsius->farenheit [c] (+ (* c (/ 9 5)) 32))
(defn farenheit->celsius [f] (* (- f 32) (/ 5 9)))

(p/defn TemperatureConverter [temperature]
  (dom/hiccup
    [:div
     [:h1 "Temperature Converter"]
     [:table
      [:tr
       [:th "Celcius"]
       [:th "Farenheit"]]
      [:tr [[:td (ui/numeric-input {:value    temperature
                                    :step     "0.5"
                                    :format   "%.2f"
                                    :on-input (map (partial conj [:celsius]))})]
            [:td (ui/numeric-input {:value    (celsius->farenheit temperature)
                                    :step     "0.5"
                                    :on-input (map (partial conj [:fahrenheit]))})]]]]]))

(defn set-state! [!state [event-tag value :as event]]
  (prn "event:" event)
  (case event-tag
    :celsius    (reset! !state value)
    :fahrenheit (reset! !state (farenheit->celsius value))))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/node (dom/by-id "root")]
                   (let [!state (atom 0)]
                     (ui/interpreter #{:celsius :fahrenheit} (partial set-state! !state)
                        (TemperatureConverter. (p/watch !state)))))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main))))