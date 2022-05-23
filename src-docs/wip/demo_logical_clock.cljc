(ns wip.demo-logical-clock
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.demo-logical-clock)))       ; forces shadow hot reload to also reload JVM at the same time

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Client local clocks"))
    (dom/p (dom/text "client"))
    (dom/dl
      (dom/dt (dom/text "time")) (dom/dd (dom/text z/time))
      (dom/dt (dom/text "clock")) (dom/dd (dom/text z/clock))
      (dom/dt (dom/text "counter")) (dom/dd (dom/text z/ticker)))
    (dom/h1 (dom/text "Remote clock streams"))
    (dom/p (dom/text "Note: this deadlocks currently due to bug in m/sleep"))
    (dom/dl
      (dom/dt (dom/text "time")) (dom/dd (dom/text ~@z/time))
      (dom/dt (dom/text "clock")) (dom/dd (dom/text ~@z/clock))
      (dom/dt (dom/text "counter")) (dom/dd (dom/text ~@z/ticker)))))

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/parent (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
