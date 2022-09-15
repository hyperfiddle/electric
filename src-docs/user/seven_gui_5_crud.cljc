(ns user.seven-gui-5-crud
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [clojure.string :as str])
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

;;; Presentation

(p/defn App []
  (p/client
    (dom/h1 "7 GUIs: CRUD") 
    (let [state (p/watch !state)
          selected (:selected state)]
      (dom/div {:style {:display :grid
                        :grid-gap "0.5rem"
                        :align-items :baseline
                        :grid-template-areas "'a b c c'\n
                                            'd d e f'\n
                                            'd d g h'\n
                                            'd d i i'\n
                                            'j j j j'"}}
        (dom/span {:style {:grid-area "a"}}
                  "Filter prefix:")
        (let [needle (::ui/value (ui/input {:style {:grid-area "b"}}))]
          (dom/ul {:style {:grid-area "d"
                           :background-color :white
                           :list-style-type :none
                           :padding 0
                           :border "1px gray solid"
                           :height "100%"}}
            (p/for [entry (filter-names (:names state) needle)]
              (let [id (key entry)
                    value (val entry)]
                (ui/element dom/li {::ui/click-event (p/fn [_] (select! id))
                                    ::dom/style {:cursor :pointer
                                                 :color (if (= selected id) :white :inherit)
                                                 :background-color (if (= selected id) :blue :inherit)
                                                 :padding "0.1rem 0.5rem"}}
                  (:surname value) ", " (:name value))))))
        (let [stage (:stage state)]
          (dom/span {:style {:grid-area "e"}} "Name:")
          (ui/input {::ui/value (:name stage)
                     ::ui/input-event (p/fn [event] (set-name! (dom/oget event :target :value)))
                     ::dom/style {:grid-area "f"}})
          (dom/span {:style {:grid-area "g"}} "Surname:")
          (ui/input {::ui/value (:surname stage)
                     ::ui/input-event (p/fn [event] (set-surname! (dom/oget event :target :value)))
                     ::dom/style {:grid-area "h"}}))
        (dom/div {:style {:grid-area "j"
                          :display :grid
                          :grid-gap "0.5rem"
                          :grid-template-columns "auto auto auto 1fr"}}
          (ui/button {::ui/click-event (p/fn [_] (create!) nil)}
            "Create")
          (ui/button {::dom/disabled (when-not selected true)
                      ::ui/click-event (p/fn [_] (update!) nil)}
            "Update")
          (ui/button {::dom/disabled (when-not selected true)
                      ::ui/click-event (p/fn [_] (delete!) nil)}
            "Delete"))))))
