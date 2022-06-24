(ns wip.demo-color
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.demo-color)))      ; forces shadow hot reload to also reload JVM at the same time


;; Goal is to show css properties are fine grained
(p/defn App []
  (dom/div {:style {:width "100px"
                    :height "100px"
                    :background-color (str "hsl(" (int (* (mod (new dom/clock 1) 60000) (/ 360 60000))) ", 100%, 70%)")}}))

(def main #?(:cljs (p/client (p/main
                              (try
                                (binding [dom/node (dom/by-id "root")]
                                  (App.))
                                (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )

