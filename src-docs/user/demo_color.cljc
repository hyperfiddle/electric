(ns user.demo-color
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros user.demo-color)))

;; Goal is to show css properties are fine grained
(p/defn App []
  (p/client
    (let [color (int (* (mod dom/system-time-secs 6) (/ 360 6)))]
      (dom/div
        {:style {:width "100px"
                 :height "100px"
                 :background-color
                 (str "hsl(" color ", 100%, 70%)")}}))))
