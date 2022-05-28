(ns wip.example-router
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.example-router)))


(def !route #?(:cljs (atom ::home)))
(p/def route (p/watch !route))                              ; client

(p/defn Button [label F]
  (let [event (dom/button
                (dom/text label)
                (dom/attribute "type" "button")
                (->> (dom/events dom/parent "click")
                     (z/impulse route)))]
    (when event
      (F. event))))

(p/defn Link [href]
  (Button.
    (name href)
    (p/fn [event]
      (js/console.log ::clicked event)
      (reset! !route href))))

(p/defn NavBar []
  (dom/div
    (Link. ::home)
    (Link. ::other)))

(p/defn Home [] (dom/div (dom/h1 (dom/text "Home"))))

(p/defn Other [] (dom/div (dom/h1 (dom/text "Other"))))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Router (minimal)"))
    (NavBar.)
    (condp = route
      ::home (Home.)
      ::other (Other.))
    (dom/dl
      (dom/dt (dom/text "route")) (dom/dd (dom/text route)))))

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/parent (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _))))))

(comment
  (user/browser-main! `main)
  )
