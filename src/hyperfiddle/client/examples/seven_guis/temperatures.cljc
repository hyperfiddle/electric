(ns hyperfiddle.client.examples.seven-guis.temperatures
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.client.examples.seven-guis.temperatures :refer [Converter Input]])))

(defn parse-num [x] #?(:cljs (-> (js/parseFloat x)
                                 (* 100)
                                 (js/Math.round)
                                 (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))

(defn to-fahrenheit [c]
  (Math/floor (+ (* c (/ 9 5)) 32)))

(defn to-celsius [f]
  (Math/floor (* (- f 32) (/ 5 9))))

(photon/defn Input [ value]
  ~(m/eduction
    (dedupe)
    #'(dom/input
       (dom/attribute "value" value)
       ;; New behavior of do makes this produce nil and the do block produces a
       ;; value. Not the same nil.
       (->> (dom/events dom/parent "keyup")
             (m/eduction (map dom/event-target)
                         (map dom/get-value)
                         (map parse-num)
                         (filter is-num?))
             ;; (m/reductions {} default-value)
             ;; value is variable, we don’t want to re-build the pipeline each
             ;; time
             ;; (m/relieve {})
             ))))

(defn log [m x] (prn m x) x)

(photon/defn Converter [temperature]
  (dom/div
   (photon/$ Input temperature)
   (dom/text " Celsius = ")
   (to-celsius (photon/$ Input (to-fahrenheit temperature)))
   (dom/text " Fahrenheit")))


(defn state [init-value]
  (let [!state (atom init-value)
        >state (m/eduction (dedupe) (m/watch !state))]
    ;; we have to dedupe maybe it’s a hack. Is there something in the language
    ;; forcing us to do that. Should we change the language?
    (fn
      ([v] (reset! !state v))
      ([notify terminate] (>state notify terminate)))))


(defn temperature []
  (photon/run
    (photon/binding [dom/parent (dom/by-id "temperature")]
      (let [>temp! (state 0)]
        (>temp! (photon/$ Converter ~>temp!))))))

(temperature)
