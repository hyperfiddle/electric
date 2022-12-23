(ns dustin.y2022.input1
  (:require #?(:cljs goog.events)
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom :refer [node]]
            [hyperfiddle.photon-ui2 :refer [Focused?]]
            [hyperfiddle.rcf :refer [tests tap % with]])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.input1)))

; recur

(p/defn Input2 [controlled-value]
  (dom/input {:type "text"}
    (loop [edit? false]
      (if edit?
        (do (recur (not (some? (Event. "blur" false))))
            (when-some [e (Event. "input" false)]
              (.-value (.-target e))))
        (do (.setAttribute dom/node "value" (str controlled-value))
            (recur (some? (Event. "focus" false)))
            controlled-value)))))

(p/defn Input2b [controlled-value]
  (dom/input {:type "text"}
    (let [input (when-some [e (Event. "input" false)]
                  (.-value (.-target e)))]
      (if (Focused?.)
        input
        (do (.setAttribute node "value" controlled-value) controlled-value)))))

; Peter

(defmacro discrete->continuous [& body])

(p/defn Input3 [controlled-value]
  (dom/input {:type "text"}
    (let [focused? (discrete->continuous false ; <- initial value
                                         (dom/events "onfocus") true ; <- on focus turn to true
                                         (dom/events "onblur") false) ; <- on blur turn to false
          input (discrete->continuous controlled-value (dom/events "oninput") (-> dom/event .-target .-value))]
      (if focused?
        input
        (do (.setAttribute node "value" controlled-value) controlled-value)))))