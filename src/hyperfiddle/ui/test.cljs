(ns hyperfiddle.ui.test
  (:require [contrib.cljs-target :refer [do-browser]]
            [hyperfiddle.photon-dom :as dom]))

(do-browser
  (defn ensure-root! []
    (when-not (dom/by-id "root")
      (let [root (.createElement js/document "div")]
        (set! (.-id root) "root")
        (.appendChild (.-body js/document) root))))

  (ensure-root!)

  (defn focus [elem]
    (.dispatchEvent elem (js/FocusEvent. "focus"))
    (.dispatchEvent elem (js/FocusEvent. "focusin" #js {:bubbles true}))
    (.focus elem #js {"focusVisible" true}))

  (defn blur [elem]
    (.dispatchEvent elem (js/FocusEvent. "blur"))
    (.blur elem))

  (defn click [elem]
    (blur (.-activeElement js/document))
    (.click elem)
    (.dispatchEvent elem (js/MouseEvent. "mousedown" #js {:bubbles true}))
    (.dispatchEvent elem (js/MouseEvent. "mouseup" #js {:bubbles true})))

  (defn press [elem key]
    (.dispatchEvent elem (js/KeyboardEvent. "keydown" #js {:bubbles true, :key key}))
    (.dispatchEvent elem (js/KeyboardEvent. "keyup" #js {:bubbles true, :key key})))

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

  )
