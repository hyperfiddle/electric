(ns user.healthcheck
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.healthcheck)))      ; forces shadow hot reload to also reload JVM at the same time

(p/defn App []
  (dom/div
    (dom/h1 "Healthcheck")
    (dom/dl
      (dom/dt "time")   (dom/dd dom/system-time-ms)
      (dom/dt "client") (dom/dd (p/client (pr-str (type 1))))
      (dom/dt "server") (dom/dd (p/server (pr-str (type 1)))))))

(def main
  #?(:cljs (p/boot
             (try
               (binding [dom/node (dom/by-id "root")]
                 (App.))
               (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )
