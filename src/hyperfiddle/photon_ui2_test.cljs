(ns hyperfiddle.photon-ui2-test
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui]
   [hyperfiddle.ui.test :as uit]
   [hyperfiddle.rcf :as rcf :refer [% tap tests with]]))

(tests "ui/input accepts literal props map"
  (with (p/run (binding [dom/node (dom/by-id "root")]
                 (ui/input "controlled-value" {:style {:width "100px"}}
                   (tap (-> dom/node .-style .-cssText)))))
    % := "width: 100px;"))

(tests "ui/input blur reverts to original value"
  (def in (atom nil))
  (def discard (p/run (binding [dom/node (dom/by-id "root")]
                        (tap (ui/input "controlled-value"
                               (reset! in dom/node))))))
  % := "controlled-value"
  (uit/focus @in)
  (uit/focused? @in) := true
  (uit/swap-value! @in (constantly "new-value"))
  % := "new-value"
  (uit/blur @in)
  % := "controlled-value"
  (discard))


(tests "ui/textarea"
  (def ta (atom nil))
  (def discard (p/run (binding [dom/node (dom/by-id "root")]
                        (tap (ui/textarea "controlled-value"
                               (reset! ta dom/node))))))
  % := "controlled-value"
  (uit/focus @ta)
  (uit/focused? @ta) := true
  (uit/swap-value! @ta (constantly "new-value"))
  % := "new-value"
  (uit/blur @ta)
  % := "controlled-value"
  (discard))

(tests "ui/checkbox"
  (def cb (atom nil))
  (def discard (p/run (binding [dom/node (dom/by-id "root")]
                        (tap (ui/checkbox false
                               (reset! cb dom/node))))))
  % := false
  (uit/focus @cb)
  (uit/toggle! @cb) % := true
  (uit/toggle! @cb) % := false
  (uit/toggle! @cb) % := true
  (uit/blur @cb)
  % := false
  (discard)
  )

(tests "ui/select"
  (def sel (atom nil))
  (def discard (p/run (binding [dom/node (dom/by-id "root")]
                        (tap (ui/select {:options [{:text ""} {:text "a"} {:text "b"} {:text "c"}]
                                         :value "a"}
                               (reset! sel dom/node))))))
  % := "a"
  (uit/swap-value! @sel (constantly "b"))
  % := "b"
  (uit/swap-value! @sel (constantly "c"))
  % := "c"
  (discard)
  )


(tests "ui/Value"
  (def in (atom nil))
  (def discard (p/run (binding [dom/node (dom/by-id "root")]
                        (tap (dom/with (dom/dom-element dom/node "input") (reset! in dom/node) (ui/Value.))))))
  % := ""
  (uit/swap-value! @in (constantly "xxx"))
  % := "xxx"
  (uit/swap-value! @in (constantly "yyy"))
  % := "yyy"
  (discard)
  )
