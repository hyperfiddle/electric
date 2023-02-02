(ns user.demo-color
  #?(:cljs (:require-macros user.demo-color))
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]))

; Goal is to show css properties are reactive at fine granularity
(p/defn App []
  (p/client
    (let [color (int (* (mod dom/system-time-secs 3) (/ 360 3)))]
      (dom/div (dom/props {:style {:width "100px"
                                   :height "100px"
                                   :background-color (str "hsl(" color ", 100%, 70%)")} ; fine grained CSS
                           })))))