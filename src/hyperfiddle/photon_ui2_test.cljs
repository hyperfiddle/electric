(ns hyperfiddle.photon-ui2-test
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui]
   [hyperfiddle.rcf :as rcf :refer [% tap tests with]]))

(tests "ui/input accepts literal props map"
  (with (p/run (binding [dom/node (dom/by-id "root")]
                 (ui/input "controlled-value" {:style {:width "100px"}}
                   (tap (-> dom/node .-style .-cssText)))))
    % := "width: 100px;"))

(defn focus [elem]
  (.dispatchEvent elem (js/FocusEvent. "focus"))
  (.focus elem #js {"focusVisible" true}))

(defn blur [elem]
  (.dispatchEvent elem (js/FocusEvent. "blur"))
  (.blur elem))

(defn focused? [elem] (= (.-activeElement js/document) elem))

(defn swap-input! [elem f & args]
  (let [v (apply f (.-value elem) args)]
    (set! (.-value elem) v)
    (.dispatchEvent elem (js/InputEvent. "input"))
    v))

(tests "ui/input blur reverts to original value"
  (def in (atom nil))
  (def discard (p/run (binding [dom/node (dom/by-id "root")]
                        (tap (ui/input "controlled-value"
                               (reset! in dom/node))))))
  % := "controlled-value"
  (focus @in)
  (focused? @in) := true
  (swap-input! @in (constantly "new-value"))
  % := "new-value"
  (blur @in)
  % := "controlled-value"
  (discard))
