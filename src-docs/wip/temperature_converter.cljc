(ns wip.temperature-converter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom3 :as dom]
            #?(:cljs [goog.string.format])
            #?(:cljs [goog.string :refer [format]])
            [wip.semicontroller])
  #?(:cljs (:require-macros [wip.semicontroller :refer [semicontroller interpreter]]))
  (:import (hyperfiddle.photon Pending Remote)))

(defn celsius->farenheit [c] (+ (* c (/ 9 5)) 32))
(defn farenheit->celsius [f] (* (- f 32) (/ 5 9)))

(defn format-num [x] #?(:cljs (format "%.2f" x)))
(defn parse-num [x] #?(:cljs (-> (js/parseFloat x) (* 100) (js/Math.round) (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))
(def parse-input (comp (map dom/target-value) (map parse-num) (filter is-num?)))

(p/defn TemperatureConverter [temperature]
  (dom/hiccup
    [:div
     [:h1 "Temperature Converter"]
     [:table
      [:tr
       [:th "Celcius"]
       [:th "Farenheit"]]
      [:tr [[:td (semicontroller :focused temperature
                   (p/fn [temperature]
                     (dom/input {:type  :number
                                 :step "0.5"
                                 :value (format-num temperature)}
                       [[:focused (not (new (dom/focus-state dom/parent)))]
                        (new (dom/events "input" (comp parse-input (map (partial conj [:celsius])))))])))]
            [:td (semicontroller :focused temperature
                   (p/fn [temperature]
                     (dom/input {:type  :number
                                 :value (format-num (celsius->farenheit temperature))}
                       [[:focused (not (new (dom/focus-state dom/parent)))]
                        (new (dom/events "input" (comp parse-input (map (partial conj [:fahrenheit])))))])))]]]]]))

(defn set-state! [!state [event-tag value :as event]]
  (prn "event:" event)
  (case event-tag
    :celsius    (reset! !state value)
    :fahrenheit (reset! !state (farenheit->celsius value))))

(def main
  #?(:cljs (p/client
             (p/main
               (try
                 (binding [dom/parent (dom/by-id "root")]
                   (let [!state (atom 0)]
                     (interpreter #{:celsius :fahrenheit} (partial set-state! !state)
                       (TemperatureConverter. (p/watch !state)))))
                 (catch Pending _)
                 (catch Remote _))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main))))