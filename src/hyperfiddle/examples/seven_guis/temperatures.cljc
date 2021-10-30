(ns hyperfiddle.examples.seven-guis.temperatures
  (:require [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.examples.seven-guis.temperatures :refer [Converter Input temperature]])))

(defn parse-num [x] #?(:cljs (-> (js/parseFloat x)
                                 (* 100)
                                 (js/Math.round)
                                 (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))

(defn to-fahrenheit [c] (Math/floor (+ (* c (/ 9 5)) 32)))

(defn to-celsius [f] (Math/floor (* (- f 32) (/ 5 9))))

(p/defn Input [value]
  (dom/input
   (dom/attribute "value" value)
   (prn "v" value)
   ;; New behavior of do makes this produce nil and the do block produces a
   ;; value. Not the same nil.
   ~(->> (dom/events dom/parent "input")
         (m/eduction (map dom/event-target)
                     (map dom/get-value)
                     (map parse-num)
                     (filter is-num?)
                     (dedupe)
                     )
         (m/reductions {} value)
         ;; value is variable, we  don’t want to re-build the pipeline each
         ;; time
         (m/relieve {})
         )))

(p/defn Converter [temperature]
  (let [!temp (atom temperature)]
    (dom/div
       (reset! !temp (p/$ Input temperature))
       (dom/text " Celsius = ")
       (reset! !temp ~@ (to-celsius ~@ (p/$ Input ~@ (to-fahrenheit temperature))))
       (dom/text " Fahrenheit"))
    ~(m/watch !temp)))


(p/def temperature
  #'~@(let [>temp! (dom/state 0)
            temp   ~>temp!]
        (>temp! ~@ (p/$ Converter temp))))

(def exports (p/vars to-celsius to-fahrenheit reset! atom prn))
