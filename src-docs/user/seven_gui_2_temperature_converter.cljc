(ns user.seven-gui-2-temperature-converter
  #?(:cljs (:require-macros user.seven-gui-2-temperature-converter))
  (:require
   [hyperfiddle.electric :as p]
   [hyperfiddle.electric-dom2 :as dom]
   [missionary.core :as m]
   [clojure.math :as math]
   [hyperfiddle.electric-ui4 :as ui]))

;; https://eugenkiss.github.io/7guis/tasks#temp

(defn c->f [c] (+ (* c (/ 9 5)) 32))
(defn f->c [f] (* (- f 32) (/ 5 9)))
(defn random-value [_] (m/sp (m/? (m/sleep 2000)) (rand-int 250)))

(p/defn App []
  (p/client
    (dom/h1 (dom/text "Temperature Converter"))
    (let [!t (atom 0), t (p/watch !t)]
      ;; turn on to see concurrent updates
      #_(reset! !t (new (p/task->cp (random-value t))))
      (dom/dl
        (dom/dt (dom/text "Celsius"))
        (dom/dd
          (ui/long (math/round t) (p/fn [v] (reset! !t v))))
        (dom/dt (dom/text "Farenheit"))
        (dom/dd
          (ui/long (math/round (c->f t)) (p/fn [v] (reset! !t (f->c v)))))))))
