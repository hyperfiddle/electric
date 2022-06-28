(ns user.seven-gui-5-crud
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [clojure.string :as str])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.seven-gui-5-crud)))

;;; Instructions
;; https://eugenkiss.github.io/7guis/tasks#crud

;;; App state

(def !state (atom {:selected nil
                   :stage    {:name    nil
                              :surname nil}
                   :names    (sorted-map 0 {:name "Emil", :surname "Hans"})}))

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
                                     (assoc :stage {:name nil, :surname nil})))))
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

(defmacro cursor [val path] `(p/deduping (get-in ~val ~path)))

;;; Presentation

(p/defn App []
  (let [state    (p/watch !state)
        selected (cursor state [:selected])]
    (dom/div {:style {:display             :grid
                      :grid-gap            "0.5rem"
                      :align-items         :baseline
                      :grid-template-areas "'a b c c'\n
                                            'd d e f'\n
                                            'd d g h'\n
                                            'd d i i'\n
                                            'j j j j'"}}
             (dom/span {:style {:grid-area "a"}}
                       (dom/text "Filter prefix:"))
             (let [needle (ui/input {:style {:grid-area "b"}})]
               (dom/ul {:style {:grid-area        "d"
                                :background-color :white
                                :list-style-type  :none
                                :padding          0
                                :border           "1px gray solid"
                                :height           "100%"}}
                       (p/for [entry (filter-names (cursor state [:names]) needle)]
                         (let [id    (key entry)
                               value (val entry)]
                           (ui/element dom/li {:style    {:cursor           :pointer
                                                          :color            (if (= selected id) :white :inherit)
                                                          :background-color (if (= selected id) :blue :inherit)
                                                          :padding          "0.1rem 0.5rem"}
                                               :on-click (p/fn [_] (select! id))}
                                       (dom/text (:surname value) ", " (:name value)))))))
             (let [stage (cursor state [:stage])]
               (dom/span {:style {:grid-area "e"}} (dom/text "Name:"))
               (ui/input {:style    {:grid-area "f"}
                          :value    (:name stage)
                          :on-input (p/fn [event] (set-name! (dom/oget event :target :value)))})
               (dom/span {:style {:grid-area "g"}} (dom/text "Surname:"))
               (ui/input {:style    {:grid-area "h"}
                          :value    (:surname stage)
                          :on-input (p/fn [event] (set-surname! (dom/oget event :target :value)))}))
             (dom/div {:style {:grid-area             "j"
                               :display               :grid
                               :grid-gap              "0.5rem"
                               :grid-template-columns "auto auto auto 1fr"}}
                      (ui/button {:on-click (p/fn [_] (create!) nil)}
                                 (dom/text "Create"))
                      (ui/button {:disabled (when-not selected true)
                                  :on-click (p/fn [_] (update!) nil)}
                                 (dom/text "Update"))
                      (ui/button {:disabled (when-not selected true)
                                  :on-click (p/fn [_] (delete!) nil)}
                                 (dom/text "Delete"))))))

(def main #?(:cljs (p/client (p/main
                              (try
                                (binding [dom/node (dom/by-id "root")]
                                  (App.))
                                (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
