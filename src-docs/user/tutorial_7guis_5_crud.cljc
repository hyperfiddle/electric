(ns user.tutorial-7guis-5-crud
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [clojure.string :as str]))

;;; Instructions
;; https://eugenkiss.github.io/7guis/tasks#crud

;;; App state

(def !state (atom {:selected nil
                   :stage {:name ""
                           :surname ""}
                   :names (sorted-map 0 {:name "Emil", :surname "Hans"})}))

;;; Business logic

(def next-id (partial swap! (atom 0) inc))

(defn select! [id]
  (swap! !state (fn [state] (assoc state :selected id, :stage (get-in state [:names id])))))

(defn set-name! [name]
  (swap! !state assoc-in [:stage :name] name))

(defn set-surname! [surname]
  (swap! !state assoc-in [:stage :surname] surname))

(defn create! [] (swap! !state (fn [{:keys [stage] :as state}]
                                 (-> state
                                   (update :names assoc (next-id) stage)
                                   (assoc :stage {:name "", :surname ""})))))
(defn delete! [] (swap! !state (fn [{:keys [selected] :as state}] (update state :names dissoc selected))))
(defn update! [] (swap! !state (fn [{:keys [selected stage] :as state}]
                                 (assoc-in state [:names selected] stage))))

(defn filter-names [names-map needle]
  (if (empty? needle)
    names-map
    (let [needle (str/lower-case needle)]
      (reduce-kv (fn [r k {:keys [name surname]}]
                   (if (or (str/includes? (str/lower-case name) needle)
                         (str/includes? (str/lower-case surname) needle))
                     r
                     (dissoc r k)))
        names-map names-map))))

;;; Presentation

(e/defn CRUD []
  (e/client
    (dom/h1 (dom/text "7 GUIs: CRUD"))
    (let [state (e/watch !state)
          selected (:selected state)]
      (dom/div (dom/props {:style {:display :grid
                                   :grid-gap "0.5rem"
                                   :align-items :baseline
                                   :grid-template-areas "'a b c c'\n
                                                         'd d e f'\n
                                                         'd d g h'\n
                                                         'd d i i'\n
                                                         'j j j j'"}})
        (dom/span (dom/props {:style {:grid-area "a"}})
          (dom/text "Filter prefix:"))
        (let [!needle (atom ""), needle (e/watch !needle)]
          (ui4/input needle (e/fn [v] (reset! !needle v))
            (dom/props {:style {:grid-area "b"}}))
          (dom/ul (dom/props {:style {:grid-area "d"
                                      :background-color :white
                                      :list-style-type :none
                                      :padding 0
                                      :border "1px gray solid"
                                      :height "100%"}})
            (e/for [entry (filter-names (:names state) needle)]
              (let [id (key entry)
                    value (val entry)]
                (dom/li (dom/text (:surname value) ", " (:name value))
                  (dom/props {:style {:cursor :pointer
                                      :color (if (= selected id) :white :inherit)
                                      :background-color (if (= selected id) :blue :inherit)
                                      :padding "0.1rem 0.5rem"}})
                  (dom/on "click" (e/fn [_] (select! id))))))))
        (let [stage (:stage state)]
          (dom/span (dom/props {:style {:grid-area "e"}}) (dom/text "Name:"))
          (ui4/input (:name stage) (e/fn [v] (set-name! v))
            (dom/props {:style {:grid-area "f"}}))
          (dom/span (dom/props {:style {:grid-area "g"}}) (dom/text "Surname:"))
          (ui4/input (:surname stage) (e/fn [v] (set-surname! v))
            (dom/props {:style {:grid-area "h"}})))
        (dom/div (dom/props {:style {:grid-area "j"
                                     :display :grid
                                     :grid-gap "0.5rem"
                                     :grid-template-columns "auto auto auto 1fr"}})
          (ui4/button (e/fn [] (create!)) (dom/text "Create"))
          (ui4/button (e/fn [] (update!)) (dom/text "Update")
            (dom/props {:disabled (not selected)}))
          (ui4/button (e/fn [] (delete!)) (dom/text "Delete")
            (dom/props {:disabled (not selected)})))))))
