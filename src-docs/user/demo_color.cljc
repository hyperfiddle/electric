(ns user.demo-color
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z])
  #?(:cljs (:require-macros user.demo-color)))

;; Goal is to show css properties are fine grained
(p/defn App []
  (p/client
    (let [color (int (* (mod z/time 6000) (/ 360 6000)))]
      (dom/div
        {:style {:width "100px"
                 :height "100px"
                 :background-color
                 (str "hsl(" color ", 100%, 70%)")}}))))
