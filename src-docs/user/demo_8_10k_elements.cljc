(ns user.demo-8-10k-elements
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.ui.color :refer [rgb hsv->rgb]]
            [missionary.core :as m])
  #?(:cljs (:require-macros user.demo-8-10k-elements)))

(defn state! [width height]
  (vec (for [y (range height) x (range width)]
         (atom 0))))

(defn perturb! [t n state]
  (dotimes [_ (Math/floor (/ n 100))]
    (reset! (get state (rand-int n)) 9)))

(p/defn Controls [!running !width n]
  (dom/dl
    (dom/dt (dom/label {::dom/for "field-running"} " running?"))
    (dom/dd (ui/checkbox {::dom/id         "field-running"
                          ::ui/value (p/watch !running)
                          ::ui/input-event (p/fn [e] (reset! !running (-> e :target :checked)))}))
    (dom/dt (dom/label {::dom/for "field-width"} "width"))
    (dom/dd
      (dom/div
        (ui/button {::ui/click-event (p/fn [e] (reset! !width 45))} "2.5k")
        (ui/button {::ui/click-event (p/fn [e] (reset! !width 89))} "10k (wait for it)")))
    (dom/dt (dom/label "cells")) (dom/dd n " (total dom elements roughly double due to text nodes)")))

(defn countdown [x]                     ; Count down to 0 then terminate.
  (m/relieve {} (m/ap (loop [x x]
                        (m/amb x
                          (if (pos? x)
                            (do (m/? (m/sleep 100))
                                (recur (dec x)))
                            x))))))

(defn cell-color [x]
  (if (> x 1) ; In photon-land, this conditional would introduce a switch and trigger a ws message for client-server frame coordination.
    (apply rgb (hsv->rgb (/ 0 360)
                 (-> x (/ 7.5) (* 1.33))
                 0.95))
    "#ddd"))

(p/defn Board [!!state running? width height n]
  (dom/div
    {:class "board"
     :style {:font-family "monospace" :font-size "9px" :margin 0 :padding 0}}
    (p/for [y (range 0 height)]
      (dom/div
        (p/for [x (range 0 width)]
          (let [i (+ x (* y height))
                !x (get !!state i) x (new (countdown (p/watch !x)))]
            (dom/span {:style {:color (cell-color x)}}
              x)))))))

(p/defn App []
  (dom/h1 "10k dom elements")
  (dom/element "style" ".board span { display: inline-block; }") ; prevent layout shifts
  (let [!running (atom true) running? (p/watch !running)
        !width (atom 45) width (p/watch !width)
        height (Math/floor (* width 0.64))
        !!state (state! width height)
        n (* width height)]

    (when running?
      (perturb! (dom/Clock. 10) n !!state))

    (Controls. !running !width n)
    (Board. !!state running? width height n)))
