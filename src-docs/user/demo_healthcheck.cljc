(ns user.demo-healthcheck
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-healthcheck)))        ; forces shadow hot reload to also reload JVM at the same time

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Healthcheck"))
    (dom/dl
      (dom/dt (dom/text "time"))   (dom/dd (dom/text dom/time))
      (dom/dt (dom/text "client")) (dom/dd (dom/text (pr-str (type 1))))
      (dom/dt (dom/text "server")) (dom/dd (dom/text ~@(pr-str (type 1)))))))

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/parent (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
