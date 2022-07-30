(ns user.demo-10k-elements
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [missionary.core :as m])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-10k-elements)))

(def width 125)
(def height 80)

(defn ticker "tick randomly at intervals up to n ms wide" [n]
  (->> (m/ap
         (loop []
           (m/amb
             (m/? (let [r (rand-int n)] (m/sleep r r)))
             (recur))))
       (m/reductions {} 0)
       (m/relieve {})))

(p/defn App []
  (dom/h1 (dom/text "10k dom elements"))
  (let [!state (atom false)
        running? (p/watch !state)]
    (ui/checkbox {::dom/id "checkbox-running"
                  ::dom/checked running?
                  ::ui/input-event (p/fn [e] (reset! !state (-> e :target :checked)))})
    (dom/label {::dom/for "checkbox-running"} (dom/text " running?"))
    (dom/div
      {:style {:font-family "courier" :font-size "9px" :margin 0 :padding 0}}
      (p/for [y (range 0 height)]
        (dom/div
          (p/for [x (range 0 width)]
            (let [delay (if running? (new (ticker 5000)) 0)]
              (dom/span {:style {:color (case (mod delay 3) 0 "red" 1 "green" 2 "blue")}}
                        (dom/text (mod delay 10))))))))))

(def main
  #?(:cljs (p/boot
             (try
               (binding [dom/node (dom/by-id "root")]
                 (App.))
               (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )
