(ns hyperfiddle.ui.test
  (:require [hyperfiddle.photon-dom :as dom]))

(defn ensure-root! []
  (when-not (dom/by-id "root")
    (let [root (.createElement js/document "div")]
      (set! (.-id root) "root")
      (.appendChild (.-body js/document) root))))

(ensure-root!)

(defn focus [elem]
  (.dispatchEvent elem (js/FocusEvent. "focus"))
  (.focus elem #js {"focusVisible" true}))

(defn blur [elem]
  (.dispatchEvent elem (js/FocusEvent. "blur"))
  (.blur elem))

(defn focused? [elem] (= (.-activeElement js/document) elem))

(defn swap-value! [elem f & args]
  (let [v (apply f (.-value elem) args)]
    (set! (.-value elem) v)
    (.dispatchEvent elem (js/InputEvent. "input"))
    (.dispatchEvent elem (js/InputEvent. "change"))
    v))

(defn set-value! [elem v] (swap-value! elem (constantly v)))

(defn toggle! [elem]
  (set! (.-checked elem) (not (.-checked elem)))
  (.dispatchEvent elem (js/Event. "change")))
