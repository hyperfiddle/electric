(ns user.demo-8-10k-elements
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.zero :as z]
            [user.util :refer [clamp]])
  #?(:cljs (:require-macros user.demo-8-10k-elements)))

(defn foo [t] (rand-int 10))

(p/defn App []
  (dom/h1 (dom/text "10k dom elements"))
  (let [!running (atom true) running? (p/watch !running)
        !width (atom 10) width (clamp (p/watch !width) 1 150) ; n=125 is 10k elements; too big number OOMs the tab
        height (Math/floor (* width 0.64))
        t (p/deduping (if running? z/time 0))]

    (dom/dl
      (dom/dt (dom/label {::dom/for "field-running"} (dom/text " running?")))
      (dom/dd (ui/checkbox {::dom/id         "field-running"
                            ::ui/value       running?
                            ::ui/input-event (p/fn [e] (reset! !running (-> e :target :checked)))}))
      (dom/dt (dom/label {::dom/for "field-width"} (dom/text "width")))
      (dom/dd (ui/input {::dom/id "field-width" ::ui/type :number ::dom/format "%.2f"
                         ::dom/step 5 ::ui/value width
                         ::ui/input-event (p/fn [e] (reset! !width (:value dom/node)))}))
      (dom/dt (dom/label (dom/text "elements"))) (dom/dd (dom/text (* width height))))

    (dom/div
      {:style {:font-family "monospace" :font-size "9px" :margin 0 :padding 0}}
      (p/for [y (range 0 height)]
        (dom/div
          (p/for [x (range 0 width)]
            (let [v (p/deduping (foo t))]
              (dom/span {:style {:font-variant-numeric "tabular-nums" :width "1em"
                                 :color                ({0 "red"} v "#ccc")}}
                        (dom/text v)))))))))
