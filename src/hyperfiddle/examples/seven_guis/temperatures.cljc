(ns hyperfiddle.examples.seven-guis.temperatures
  (:require [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m]
            #?(:cljs [goog.string :refer [format]])
            #?(:cljs [goog.string.format]))
  #?(:cljs (:require-macros [hyperfiddle.examples.seven-guis.temperatures :refer [Input main]])))

(defn parse-num [x] #?(:cljs (-> (js/parseFloat x)
                                 (* 100)
                                 (js/Math.round)
                                 (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))

(defn format-num [[x u] unit]
  (format "%.2f"
    (if (= u unit)
      x (case [u unit]
          [:celsius :fahrenheit] (+ (* x (/ 9 5)) 32)
          [:fahrenheit :celsius] (* (- x 32) (/ 5 9))))))

(p/defn Input [value unit]
  (dom/input
    (dom/set-value! dom/parent (format-num value unit))
    [(z/target value
       (->> (dom/events dom/parent "change")
         (m/relieve {})
         (m/eduction
           (map dom/event-target)
           (map dom/get-value)
           (map parse-num)
           (filter is-num?)))) unit]))

(p/def main
  #'(let [!temp (atom [20 :celsius])
          temp ~(m/watch !temp)]
      (dom/div
        (reset! !temp (p/$ Input temp :celsius))
        (dom/text " Celsius = ")
        (reset! !temp (p/$ Input temp :fahrenheit))
        (dom/text " Fahrenheit"))))
