(ns hyperfiddle.client.examples.seven-guis.temperatures
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [devcards.core :as dc]
            [missionary.core :as m]
            [hyperfiddle.client.examples.card :refer [dom-node]])
  #?(:cljs (:require-macros [hyperfiddle.client.examples.seven-guis.temperatures :refer [Converter Input]])))

(defn inc-reducer [r _] (inc r))

(defn parse-num [x] #?(:cljs (-> (js/parseFloat x)
                                 (* 100)
                                 (js/Math.round)
                                 (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))

(defn to-fahrenheit [c]
  (Math/floor (+ (* c (/ 9 5)) 32)))

(defn to-celsius [f]
  (Math/floor (* (- f 32) (/ 5 9))))

(defn cas! [!atom v]
  #?(:cljs (js/console.log v @!atom))
  (when (not= v @!atom)
    (reset! !atom v)))

(photon/defn Input [default-value value]
  ~(m/eduction
    (dedupe)
    #'(dom/input
       (dom/attribute "value" value) ;; New behavior of do makes this produce nil
       ;; and the do block produces a value. Not the
       ;; same nil.
       ~(->> (dom/events dom/parent "keyup")
             (m/eduction (map dom/event-target)
                         (map dom/get-value)
                         (map parse-num)
                         (filter is-num?))
             (m/reductions {} default-value) ;; value is variable, we don’t want to re-build the
             ;; pipeline each time
             (m/relieve {})
             ))))

(defn log [m x] (prn m x) x)

(photon/defn Converter []
  (let [!c (atom 0)
        !f (atom 0)]
    (dom/div
     (->> (photon/$ Input 0 (log "c" ~(m/watch !c)))
          (log "c2")
          (to-fahrenheit)
          (log "c -> f")
          (reset! !f))
     (dom/text " Celsius = ")
     (->> (photon/$ Input (to-fahrenheit 0) ~(m/watch !f))
          (log "f")
          (to-celsius)
          (log "f -> c")
          (reset! !c))
     (dom/text " Fahrenheit"))))

(dc/defcard converter
  "# 2 — Temperature Converter

   Challenges: bidirectional data flow, user-provided text input.

   ![](https://eugenkiss.github.io/7guis/static/tempconv.de9aff1f.png)


   The task is to build a frame containing two textfields TC and TF representing
   the temperature in Celsius and Fahrenheit, respectively. Initially, both TC
   and TF are empty. When the user enters a numerical value into TC the
   corresponding value in TF is automatically updated and vice versa. When the
   user enters a non-numerical string into TC the value in TF is not updated and
   vice versa. The formula for converting a temperature C in Celsius into a
   temperature F in Fahrenheit is `C = (F - 32) * (5/9)` and the dual direction is
   `F = C * (9/5) + 32`.

   Temperature Converter increases the complexity of Counter by having
   bidirectional data flow between the Celsius and Fahrenheit inputs and the
   need to check the user input for validity. A good solution will make the
   bidirectional dependency very clear with minimal boilerplate code.

   Temperature Converter is inspired by the Celsius/Fahrenheit converter from
   the book Programming in Scala. It is such a widespread example—sometimes also
   in the form of a currency converter—that one could give a thousand
   references. The same is true for the Counter task."
  (dom-node
   (fn [_ node]
     (photon/run
       (photon/binding [dom/parent node]
         (photon/$ Converter))))))

(hash-map)
