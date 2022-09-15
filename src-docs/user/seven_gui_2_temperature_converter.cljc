(ns user.seven-gui-2-temperature-converter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:cljs (:require-macros user.seven-gui-2-temperature-converter)))

;; https://eugenkiss.github.io/7guis/tasks#temp

(defn celsius->farenheit [c] (+ (* c (/ 9 5)) 32))
(defn farenheit->celsius [f] (* (- f 32) (/ 5 9)))

(p/defn App []
  (p/client
    (dom/h1 "Temperature Converter")
    (let [!state (atom 0)
          temperature (p/watch !state)]
      (dom/dl
        (dom/dt "Celcius")
        (dom/dd (let [!idle (atom false)
                      idle (p/watch !idle)]
                  (ui/input {::ui/type :number
                             ::ui/value (if idle (p/current temperature) temperature)
                             ::dom/step 0.5
                             ::ui/format "%.2f"
                             ::ui/input-event (p/fn [e]
                                                (let [f (-> e :target :value js/parseFloat)]
                                                  (reset! !state f)))
                             ::ui/focus-event (p/fn [_] (reset! !idle true))
                             ::ui/blur-event (p/fn [_] (reset! !idle false))})))
        (dom/dt "Farenheit")
        (dom/dd (let [!idle (atom false)
                      idle (p/watch !idle)]
                  (ui/input {::ui/type :number
                             ::ui/value (let [f (celsius->farenheit temperature)]
                                          (if idle (p/current f) f))
                             ::dom/step 0.5
                             ::ui/focus-event (p/fn [_] (reset! !idle true))
                             ::ui/blur-event (p/fn [_] (reset! !idle false))
                             ::ui/input-event (p/fn [e]
                                                (let [f (-> e :target :value js/parseFloat)]
                                                  (reset! !state (farenheit->celsius f))))})))))))
