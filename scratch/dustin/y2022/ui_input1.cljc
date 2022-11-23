(ns dustin.y2022.ui-input1
  (:require #?(:cljs goog.events)
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom :refer [node]]
            [hyperfiddle.rcf :refer [tests tap % with]])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.ui-input1)))


(p/defn Input [controlled-value]
  ; data State = Editing local-value | NotEditing controlled-value
  (:value ; local or controlled
    (with (dom-element node "input")
      (.setAttribute node "type" "text")
      (p/with-cycle [state {:edit? false}]
        (if (:edit? state)
          (merge state
                 {:edit? (not (some? (Event. "blur" false)))}
                 (when-some [e (Event. "input" false)]
                   {:value (.-value (.-target e))})) ; use local value
          (do (.setAttribute node "value" (str controlled-value)) ; throw away local value
              {:edit? (some? (Event. "focus" false)) ; never busy - process synchronously
               :value controlled-value})))))) ; throw away local value
; the input is stable because at some point the user stops typing
; What prevents the infinite loop is at some point the state is stable, no events
; update the state and due to work skipping nothing happens.

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

(p/defn Focused? []
  (p/with-cycle [s true]
    (let [blur (not (some? (Event. "blur" false)))
          focus (some? (Event. "focus" false))]
      (case s
        true (if blur false true)
        false (if focus true false)))))

(p/defn Input2b [controlled-value]
  (dom/input {:type "text"}
    (let [input (when-some [e (Event. "input" false)]
                  (.-value (.-target e)))]
      (if (Focused?.)
        input
        (do (.setAttribute node "value" controlled-value) controlled-value)))))

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