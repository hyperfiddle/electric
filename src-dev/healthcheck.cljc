(ns healthcheck
  #?(:cljs (:require-macros healthcheck))
  (:import (hyperfiddle.photon Pending))
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom1]
            [hyperfiddle.photon-dom2 :as dom]))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Healthcheck"))
    (dom/dl
      (dom/dt (dom/text "time"))   (dom/dd (dom/text dom1/system-time-secs))
      (dom/dt (dom/text "client")) (dom/dd (dom/text (p/client (pr-str (type 1)))))
      (dom/dt (dom/text "server")) (dom/dd (dom/text (p/server (pr-str (type 1))))))))

(def main
  #?(:cljs (p/boot
             (try
               (binding [dom1/node (dom/by-id "root")]
                 (App.))
               (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )
