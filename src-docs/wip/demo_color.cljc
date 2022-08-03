(ns wip.demo-color
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.demo-color)))


;; Goal is to show css properties are fine grained
(p/defn App []
  (dom/div {:style {:width "100px"
                    :height "100px"
                    :background-color (str "hsl(" (int (* (mod (new dom/clock 1) 60000) (/ 360 60000))) ", 100%, 70%)")}}))
