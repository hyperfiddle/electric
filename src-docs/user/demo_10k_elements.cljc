(ns user.demo-10k-elements
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.zero :as z]
            [missionary.core :as m])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-10k-elements)))

(defn foo [t]
  (rand-int 10))

(p/defn App []
  (dom/h1 (dom/text "10k dom elements"))
  (let [!running (atom true) running? (p/watch !running)
        t (if running? z/time 0)]
    (ui/checkbox {::dom/id         "checkbox-running"
                  ::ui/value       running?
                  ::ui/input-event (p/fn [e] (reset! !running (-> e :target :checked)))})
    (dom/label {::dom/for "checkbox-running"} (dom/text " running?"))
    (let [width (::ui/value (ui/input {::ui/type :number ::dom/format "%.2f" ::dom/step 5 ::ui/value 40}))
          height (Math/floor (* width 0.64))]
      (dom/div (dom/text (* width height)) (dom/text " elements"))
      (dom/div
        {:style {:font-family "courier" :font-size "9px" :margin 0 :padding 0}}
        (p/for [y (range 0 height)]
          (dom/div
            (p/for [x (range 0 width)]
              (let [v (foo t)]
                (dom/span {:style {:color (case v 0 "red" "#ccc")}}
                          (dom/text v))))))))))

(def main
  #?(:cljs (p/boot
             (try
               (binding [dom/node (dom/by-id "root")]
                 (App.))
               (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )
