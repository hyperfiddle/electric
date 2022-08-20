(ns user.demo-8-10k-elements
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.ui.color :refer [rgb hsv->rgb]]
            [missionary.core :as m])
  #?(:cljs (:require-macros user.demo-8-10k-elements)))


(p/defn Controls [!n n]
  (dom/dl
    (dom/dt (dom/label {::dom/for "field-width"} "cells"))
    (dom/dd
      (dom/div
        (ui/button {::ui/click-event (p/fn [e] (reset! !n 1000))} "1k")
        (ui/button {::ui/click-event (p/fn [e] (reset! !n 10000))} "10k (wait for it)")
        (ui/button {::ui/click-event (p/fn [e] (swap! !n + 5000))} "+5k")
        (ui/input {::ui/type :number ::ui/value n
                   ::dom/disabled true
                   ::ui/input-event (p/fn [e] (reset! !n (.. e -target -value)))})))))

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
    "#eee"))

;#?(:cljs (defn x [!el]
;           (m/observe (fn mount [!]
;                        (let [!o (js/MutationObserver !)]
;                          (.observe !o !el #js {"attributes" true})
;                          (fn unmount [] (.disconnect !o)))))))

(p/defn Board [n]
  ; fixed width font + inline-block optimizes browser layout
  (dom/element "style" ".board div { width: 1em; height: 1em; display: inline-block; border: 1px #eee solid; }")
  (dom/div {:class "board" :style {:font-family "monospace" :font-size "7px" ; font-size is pixel size
                                   :margin 0 :padding 0 :line-height 0}}
    (p/for [_ (range 0 n)]
      (ui/element dom/div {::ui/mouseover-event (p/fn [e]
                                                  #_(new (countdown 9))
                                                  (dom/set-property! dom/node "style" {:background-color (cell-color 2)}))}
        #_"don't allocate text node"))))

(p/defn App []
  (dom/h1 "10k dom elements")
  (let [!n (atom 1000) n (p/watch !n)]
    (Controls. !n n)
    (Board. n)))
