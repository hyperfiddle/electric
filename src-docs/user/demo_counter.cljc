(ns user.demo-counter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-counter)))            ; forces shadow hot reload to also reload JVM at the same time


(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Counter"))
    (let [x (dom/button
              (dom/text "click me")
              (dom/set-attribute! dom/parent "type" "button")
              (new (->> (dom/events dom/parent "click")
                        (m/eduction (map (constantly 1)))
                        (m/reductions +))))]
      (dom/div
        (dom/span (dom/text "count: "))
        (dom/span (dom/text (str x)))))))

(def main #?(:cljs (p/client (p/main
                               (binding [dom/parent (dom/by-id "root")]
                                 (try
                                   (App.)
                                   (catch Pending _)))))))

(comment
  #?(:clj (def dispose (user/browser-main! `main)))
  )
