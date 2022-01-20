(ns hyperfiddle.examples.seven-guis.temperatures
  (:require [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m]
            #?(:cljs [goog.string :refer [format]])
            #?(:cljs [goog.string.format]))
  #?(:cljs (:require-macros [hyperfiddle.examples.seven-guis.temperatures :refer [temp unit input app main]])))

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

(p/def temp)
(p/def unit)

(p/def input
  #'(dom/input
      (dom/set-value! dom/parent (format-num temp unit))
      (when-some [value (z/target temp
                          (->> (dom/events dom/parent "change")
                            (m/relieve {})
                            (m/eduction
                              (map dom/event-target)
                              (map dom/get-value)
                              (map parse-num)
                              (filter is-num?))))]
        [value unit])))

(defn some-of
  ([])
  ([x] x)
  ([x y] (if (nil? x) y x))
  ([x y & zs]
   (if (nil? x)
     (loop [y y zs zs]
       (if (nil? y)
         (let [[z & zs] zs]
           (if (nil? zs)
             z (recur z zs)))
         y)) x)))

(p/def app
  #'(dom/div
      (some-of
        (binding [unit :celsius] ~input)
        (dom/text " Celsius = ")
        (binding [unit :fahrenheit] ~input)
        (dom/text " Fahrenheit"))))

(p/def main
  #'(let [!temp (atom [20 :celsius])]
      (binding [temp ~(m/watch !temp)]
        (when-some [t ~app]
          (reset! !temp t)))))
