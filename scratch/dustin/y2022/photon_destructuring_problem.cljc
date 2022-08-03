(ns dustin.y2022.photon-destructuring-problem
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros dustin.y2022.photon-destructuring-problem)))


(def !x #?(:clj (atom 0)))
(p/def x ~@(p/watch !x))

(p/defn Button [F]
  (let [[t event] (dom/button {:type "button"}
                    (dom/text "click me")
                    ; damage the DAG on purpose with a foreign clojure collection
                    [z/time (p/impulse x (dom/>events "click"))])]                 ; x has latency
    (when event
      ; this will update N times due to latency being slower than z/time
      (F. event))))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Button with server callback"))
    (dom/dl
      (dom/dt (dom/text "client button")) (dom/dd (Button. (p/fn [event]
                                                             (js/console.log ::clicked)
                                                             ~@(swap! !x inc))))
      (dom/dt (dom/text "server atom")) (dom/dd (dom/text x))
      (dom/dt (dom/text "time")) (dom/dd (dom/text z/time)))))

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/parent (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _))))))

(comment
  (user/browser-main! `main)
  )
