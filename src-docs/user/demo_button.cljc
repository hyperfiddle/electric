(ns user.demo-button
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-button)))


; This pattern works, but it is not the final idiom! Issues with this pattern:
;  - if the button is clicked faster than the browser can render, clicks will be dropped
;  - button should be disabled until server effect is acknowledged as successful

(p/defn Button [F]
  (let [event (dom/button
                (dom/text "click me")
                (dom/attribute "type" "button")
                (->> (dom/events dom/parent "click")
                     (z/impulse dom/time)))]
    (when event
      (F. event))))

(def !x #?(:clj (atom 0)))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Button with server callback"))
    (dom/dl
      (dom/dt (dom/text "client button")) (dom/dd (Button. (p/fn [event]
                                                             (js/console.log ::clicked event)
                                                             ~@(swap! !x inc))))
      (dom/dt (dom/text "server atom")) (dom/dd (dom/text ~@(p/watch !x))))))

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/parent (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _))))))

(comment
  (user/browser-main! `main)
  )
