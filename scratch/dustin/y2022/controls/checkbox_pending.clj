(ns dustin.y2022.controls.checkbox-pending
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [user.demo-5-todomvc :as t]
            [datascript.core :as d])
  (:import hyperfiddle.photon.Pending))

; Leo
(p/defn CheckBox [class checked Monitor]
  (p/with-state false ; data State = Idle | Pending
    (dom/input {:type "checkbox"
                :class class
                :checked checked
                :aria-busy p/state ; Pending
                :on-input (fn [_] (p/change! {} true))}
               (when p/state
                 (when (try (Monitor. (.-checked dom/node)) (catch Pending _))
                   (p/change! {} false)))
               dom/node)))

; Dustin first refactor
(p/defn CheckBox [class checked Change!]
  (p/with-state false
    (dom/input {:type "checkbox"
                :class class
                :checked checked
                :aria-busy p/state
                :on-input (p/fn [_]
                            (p/change! {} true)
                            (when (try (Change! (.-checked dom/node)) (catch Pending _))
                              (p/change! {} false)))}
               dom/node)))

(p/defn CheckBox [class checked Change!]
  (let [!x (atom false) x (p/watch !x)]
    (dom/input {:type "checkbox"
                :class class
                :checked checked
                :aria-busy x
                :on-input (p/fn [_]
                            (reset! !x true)
                            (when (try (Change! (.-checked dom/node)) (catch Pending _))
                              (reset! !x false)))}
               dom/node)))


; Dustin final refactor
(p/defn CheckBox [class checked Change!]
  (p/with-state false
    (dom/span {:class (if p/state "user-busy")}
      (dom/input {:type "checkbox"
                  :class class
                  :checked checked
                  :aria-busy p/state
                  :on-input (p/fn [_] (Change! (.-checked dom/node)))}))))

; gone too far
(p/defn CheckBox [class checked Change!]
  (dom/input {:type "checkbox"
              :class class
              :checked checked
              ;:aria-busy p/busy
              :on-input (p/fn [_] (Change! (.-checked dom/node)))}))