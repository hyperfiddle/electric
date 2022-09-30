(ns hyperfiddle.event
  (:refer-clojure :exclude [type])
  (:require [hyperfiddle.rcf :as rcf :refer [% tests with tap]]))

(defn ->event [typ data] (js/CustomEvent. typ #js {:detail data}))
(defn type [evt] (.-type evt))
(defn data [evt] (.-detail evt))
(defn dispatch [target evt] (.dispatchEvent target evt))
(defn listen [target typ f]
  (.addEventListener target typ f)
  #(.removeEventListener target typ f))

(tests
  (let [evt (->event "foo" {:x 1})]
    (type evt) := "foo"
    (data evt) := {:x 1})

  (with (listen js/window "foo" tap)
    (let [evt (->event "foo" {:x 1})]
      (dispatch js/window evt)
      % := evt)))
