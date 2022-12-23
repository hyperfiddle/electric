(ns dustin.y2022.ui.hiccup-comparison
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom :refer [h]]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.y2022.ui.hiccup-comparison)))

"reagent"
(defn simple-component []
  [:div
   [:p "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red "] "text."]])

"dom/h"
(p/defn Simple-component []
  (dom/h [:div
          [:p "I am a component!"]
          [:p.someclass "I have " [:strong "bold"]
           [:span {:style {:color "red"}} " and red "] "text."]]))


(defn hello-component [name]
  [:p "Hello, " name "!"])



(p/defn Hello-component [name]
  (dom/h [:p "Hello, " name "!"]))

(p/defn Say-hello []
  (Hello-component. "world"))

(declare defui)


; Geoffrey demo

"photon today"
(p/defn Hello-component [name]
  (dom/div "42"
    (let []
      (dom/p "Hello, " name "!"))))

(p/defn Say-hello []
  (Hello-component. "world"))



"photon today"
(p/defn Hello-component [name]
  (Div. "42"
        (let []
          (P. "Hello, " name "!"))))

(p/defn Say-hello []
  (Hello-component. "world"))



"reagent"
(defn hello-component [name]
  [:div "42"
   (let []
     [:p "Hello, " name "!"])])

(defn say-hello []
  [hello-component "world"])



"uix"
(defui hello-component [name]
  ($ :div "42"
     (let []
       ($ :p "Hello, " name "!"))))

(defui say-hello []
  ($ hello-component "world"))



"leo proposal"
(p/defn Hello-component [name]
  (dom/h :div "42"
    (let []
      (dom/h :p "Hello, " name "!"))))

(p/defn Say-hello [name]
  (Hello-component. "world"))



"leo proposal alternate"
(p/defn Hello-component [name]
  (h :div "42"
    (let []
      (h :p "Hello, " name "!"))))

(p/defn Say-hello [name]
  (Hello-component. "world"))


"leo proposal alternate"
(p/defn-hiccup Hello-component [name]
               [:div "42"
                (let []
                  [:p "Hello, " name "!"])])

(p/defn Say-hello [name]
  (Hello-component. "world"))



"photon today"
(p/defn Hello-component [name]
  <div "42"
  (let []
    <p "Hello, " name "!">))

(p/defn Say-hello []
  (Hello-component. "world"))




















(def types (mapv (fn [k] {:text (name k) ::value k}) ["" ::admin ::district]))
(p/defn Index []
  (p/with-state {::search "" ::type-idx 0}
                (p/server (gridsheet/GridSheet. (query. search (::value (nth types (::type-idx p/state))) *db*) {}))
                (dom/h :input {:on-input (fn [e] (dom/change! assoc ::search (.-value (.-target e))))})
                (dom/h :select {:on-input (fn [e] (dom/change! assoc ::type-idx (js/parseInt (.-value (.-target e)) 10)))}
                  (p/for [[i {:keys [text]}] (map-indexed vector types)]
                    (dom/h :option {:value (str i)} text)))))

(p/defn Index []
  (p/with-state [search ""
                 type-idx 0]
                (p/server (gridsheet/GridSheet. (query. search (::value (nth types type-idx)) *db*) {}))
                (dom/h :input {:on-input (fn [e] (dom/change! assoc ::search (.-value (.-target e))))})
                (dom/h :select {:on-input (fn [e] (dom/change! type-idx assoc ::type-idx (js/parseInt (.-value (.-target e)) 10)))}
                  (p/for [[i {:keys [text]}] (map-indexed vector types)]
                    (dom/h :option {:value (str i)} text)))))


(def types (mapv (fn [k] {:text (name k) ::value k}) ["" ::admin ::district]))
(p/defn Index []
  (let [!search (atom "") search (p/watch !search)
        !type-idx (atom 0) type-idx (p/watch !type-idx)]
    (p/server (gridsheet/GridSheet. (index. search (::value (get types type-idx)) *db*) {}))
    (ui/input {::ui/input-event (p/fn [e] (reset! !search (.. e -target -value)))})
    (ui/select {::ui/value (get types type-idx)
                ::ui/options types
                ::ui/change-event (p/fn [[_e v]] (reset! !type-idx (.indexOf types v)))})))