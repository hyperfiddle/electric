(ns hyperfiddle.photon-ui2-test
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui]
   [hyperfiddle.rcf :as rcf :refer [% tap tests]]
   [missionary.core :as m])
  (:require-macros hyperfiddle.photon-ui2-test))

(tests "ui/input accepts literal props map"
  (run (ui/input "controlled-value" {:style {:width "100px"}}
         (tap (-> dom/node .-style .-cssText))))
  % := "width: 100px;")

;; TODO figure this out
#_
(tests "ui/input blur reverts to original value"
  (def in (atom nil))
  (def discard (p/run (binding [dom/node (dom/by-id "root")]
                        (tap (ui/input "controlled-value"
                               (reset! in dom/node))))))
  ((m/reduce (fn [_ _] (tap :focused)) nil (m/observe (fn [!]
                                                        (.addEventListener @in "focus" !)
                                                        #(.removeEventListener @in "focus" !))))
   identity identity)
  % := "controlled-value"
  (.focus @in)
  % := :focused
  ;; this fails, probably async?
  ;; (.-activeElement js/window) := @in
  ;; (set! (.-value @in) "new-value")
  ;; or (.dispatchEvent @in (js/KeyboardEvent. #js {:key "x"}))
  ;; % := "new-value"
  ;; (.blur @in)
  ;; % := "controlled-value"
  (discard))
