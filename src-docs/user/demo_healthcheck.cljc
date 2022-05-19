(ns user.demo-healthcheck
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            devkit)
  #?(:cljs (:require-macros user.demo-healthcheck))        ; forces shadow hot reload to also reload JVM at the same time
  (:import (hyperfiddle.photon Pending)))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Healthcheck"))
    (dom/p (dom/span (dom/text "millisecond time: "))
           (dom/span (dom/text dom/time)))
    (dom/p (dom/div (dom/text ~@(pr-str (type 1))))
           (dom/div (dom/text (pr-str (type 1)))))))

(def main #?(:cljs (p/client (p/main
                               (try (binding [dom/parent (dom/by-id "root")]
                                      (App.))
                                 (catch Pending _))))))

(comment
  #?(:clj (def dispose (devkit/main :main `main)))
  )
