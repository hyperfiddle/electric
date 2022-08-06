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

(defn animate! [!x]
  (m/ap (m/amb 0 (loop []
                   (m/? (m/sleep 100))
                   (swap! !x dec)
                   (m/amb (recur))))))

(defn perturb! [t n state]
  (dotimes [_ (Math/floor (/ n 100))]
    (reset! (get state (rand-int n)) 9)))

(p/defn App []
  (dom/h1 (dom/text "10k dom elements"))
  (let [!running (atom true) running? (p/watch !running)
        !width (atom 30) width (p/watch !width)
        height (Math/floor (* width 0.64))
        !!state (state! width height)
        n (* width height)]

    (when-let [t (if running? (dom/Clock. 10) nil)]
      (perturb! t n !!state))

    (dom/dl
      (dom/dt (dom/label {::dom/for "field-running"} (dom/text " running?")))
      (dom/dd (ui/checkbox {::dom/id         "field-running"
                            ::ui/value       running?
                            ::ui/input-event (p/fn [e] (reset! !running (-> e :target :checked)))}))
      (dom/dt (dom/label {::dom/for "field-width"} (dom/text "width")))
      (dom/dd
        (dom/div
          (ui/button {::ui/click-event (p/fn [e] (reset! !width 29))} (dom/text "1k"))
          (ui/button {::ui/click-event (p/fn [e] (reset! !width 45))} (dom/text "2.5k"))
          (ui/button {::ui/click-event (p/fn [e] (reset! !width 89))} (dom/text "10k (wait for it, ~10s, 1g allocated)"))))
      (dom/dt (dom/label (dom/text "cells"))) (dom/dd (dom/text n) (dom/text " (total dom elements roughly double due to text nodes)")))

    (dom/div
      {:style {:font-family "monospace" :font-size "9px" :margin 0 :padding 0}}
      (p/for [y (range 0 height)]
        (dom/div
          (p/for [x (range 0 width)]
            (let [i (+ x (* y height))
                  !x (get !!state i) x (p/watch !x)]
              (dom/span {:style {:color                (if (> x 1)
                                                         (apply rgb (hsv->rgb (/ i n) #_(/ 0 360)
                                                                              (-> x (/ 7.5) (* 1.33))
                                                                              0.95))
                                                         "#ddd")}}
                        (dom/text x))
              (when (> x 0)
                (new (animate! !x))))))))))
