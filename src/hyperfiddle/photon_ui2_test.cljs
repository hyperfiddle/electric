(ns hyperfiddle.photon-ui2-test
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui]
   [hyperfiddle.ui.test :as uit]
   [hyperfiddle.rcf :as rcf :refer [% tap tests with]])
  (:require-macros [hyperfiddle.photon-ui2-test :refer [setup]]))

(def it (atom nil))

(tests "ui/input accepts literal props map"
  (with (p/run (binding [dom/node (dom/by-id "root")]
                 (ui/input "controlled-value" {:style {:width "100px"}}
                   (tap (-> dom/node .-style .-cssText)))))
    % := "width: 100px;"))

(tests "ui/input blur reverts to original value"
  (def discard (setup (ui/input "controlled-value")))
  % := "controlled-value"
  (uit/focus @it)
  (uit/focused? @it) := true
  (uit/set-value! @it "new-value")
  % := "new-value"
  (uit/blur @it)
  % := "controlled-value"
  (discard))


(tests "ui/textarea"
  (def discard (setup (ui/textarea "controlled-value")))
  % := "controlled-value"
  (uit/focus @it)
  (uit/focused? @it) := true
  (uit/set-value! @it "new-value")
  % := "new-value"
  (uit/blur @it)
  % := "controlled-value"
  (discard))

(tests "ui/checkbox"
  (def discard (setup (ui/checkbox false)))
  % := false
  (uit/focus @it)
  (uit/toggle! @it) % := true
  (uit/toggle! @it) % := false
  (uit/toggle! @it) % := true
  (uit/blur @it)
  % := false
  (discard)
  )

(tests "ui/select"
  (def discard (setup (ui/select [{:text ""} {:text "a"} {:text "b"} {:text "c"}] "a")))
  % := "a"
  (uit/set-value! @it "b")
  % := "b"
  (uit/set-value! @it "c")
  % := "c"
  (discard)
  )


(tests "ui/Value"
  (def in (atom nil))
  (def discard (p/run (binding [dom/node (dom/by-id "root")]
                        (tap (dom/with (dom/dom-element dom/node "input") (reset! in dom/node) (ui/Value.))))))
  % := ""
  (uit/set-value! @in "xxx")
  % := "xxx"
  (uit/set-value! @in "yyy")
  % := "yyy"
  (discard)
  )

(tests "ui/long"
  (def discard (setup (ui/long 0)))
  % := 0
  (uit/focus @it)
  (uit/set-value! @it "1")
  % := 1
  (uit/set-value! @it "xyz")           ; nothing, not a number
  (uit/set-value! @it "2")
  % := 2
  (uit/blur @it)
  % := 0
  (discard)
  )

(tests "ui/double"
  (def discard (setup (ui/double 1.1 pr-str)))
  % := 1.1
  (uit/focus @it)
  (uit/set-value! @it 2.2)
  % := 2.2
  (uit/set-value! @it "xyz")            ; nothing, not a number
  (uit/set-value! @it 3.3)
  % := 3.3
  (uit/blur @it)
  % := 1.1
  (discard)
  )
