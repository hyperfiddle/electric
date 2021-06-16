(ns leo.ui
  (:require [missionary.core :as m]))

(declare defnode)
(declare dom-element)
(declare mount!)

(defn dom-change [e]
  #?(:cljs (fn [!]
             (goog.events/addEventListener e !)
             #(goog.events/removeEventListener e !))))

(defn dom-option [value]
  (.createElement js/document "option" value))

(defn set-value! [e x]
  #?(:cljs (.setAttribute e "value" x)))

(defnode form-input [parent init]
  (let [e (dom-element "input" {:type "text"})]
    (mount! parent e)
    (set-value! e init)
    ~(m/observe (dom-change e))))

;; Q? how to abstract away the dom ?

(defnode form-select [parent init & opts]
  (let [e (dom-element "input" {:type "select"})]
    (for [o opts]
      (mount! e (dom-option o)))
    (mount! parent e)
    (set-value! e init)
    ~(m/observe (dom-change e))))