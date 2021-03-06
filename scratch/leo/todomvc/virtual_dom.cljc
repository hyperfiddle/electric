(ns leo.todomvc.virtual-dom
  (:require [leo.hfdl :refer [ifn $]]
            [missionary.core :as m]))

(def state (atom []))

(defn enter? [e]
  (comment TODO))

(defn update-description [todos id description]
  (comment TODO))

(def todo-item
  (ifn [{:keys [id description done? editing?]}]
    [:div {}
     [:input {:type "checkbox" :value done?}]
     (if editing?
       [:span description]
       [:input-text
        {:type "text"
         :value description
         :on-keydown (fn [e] (when (enter? e) (swap! state update-description id (.-value e))))}])]))

(def todo-list
  (ifn [todos]
    (rfor [todo :db/id todos]
      ($ todo-item todo))))

(defn render [parent >hiccup]
  (comment TODO return flow))

(defn main []
  (run-dag
    @(render (.-body js/document) ($ todo-list @(m/watch state)))))