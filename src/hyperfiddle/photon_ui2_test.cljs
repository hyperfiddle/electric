(ns hyperfiddle.photon-ui2-test
  (:require
   [contrib.cljs-target :refer [do-browser]]
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui]
   [hyperfiddle.ui.test :as uit]
   [hyperfiddle.rcf :as rcf :refer [% tap tests with]])
  (:require-macros [hyperfiddle.photon-ui2-test :refer [setup]]))

(do-browser

  (def it (atom nil))

  (tests "ui/input accepts literal props map"
    (with (p/run (binding [dom/node (dom/by-id "root")]
                   (ui/input "controlled-value" {:style {:width "100px"}}
                             (tap (-> dom/node .-style .-cssText)))))
      % := "width: 100px;"))

  #_ ; test broken by https://github.com/hyperfiddle/photon/commit/9793ff8b8d7c7d9c428f97b2192536f2fdd37994
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
  
  #_ ; test broken by https://github.com/hyperfiddle/photon/commit/9793ff8b8d7c7d9c428f97b2192536f2fdd37994
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

  (tests "ui/select returns the selected value"
    (def !value (atom nil))
    (def discard (setup (ui/select [{:text "" :value nil}
                                    {:text "a" :value :a}
                                    {:text "b" :value :b}
                                    {:text "c" :value :c}]
                                   (p/watch !value)
                                   )))
    % := nil
    (reset! !value :b)
    % := :b
    (reset! !value :c)
    % := :c
    (discard)
    )

  (tests "ui/select indentifies options by index (internal)"
    (def discard (setup (ui/select [{:text "" :value nil}
                                    {:text "a" :value :a}
                                    {:text "b" :value :b}
                                    {:text "c" :value :c}]
                                   :a)))
    % := :a
    (uit/set-value! @it 2)
    % := :b
    (uit/set-value! @it 3)
    % := :c
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
    (def discard (setup (ui/double 1.1)))
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

  (tests "ui/keyword"
    (def discard (setup (ui/keyword :foo)))
    % := :foo
    (uit/focus @it)
    (uit/set-value! @it :bar)
    % := :bar
    (uit/set-value! @it "baz")            ; nothing, not a keyword
    (uit/set-value! @it ":quux")
    % := :quux
    (uit/set-value! @it ":foo/bar")
    % := :foo/bar
    (uit/blur @it)
    % := :foo
    (discard)
    )

  (tests "ui/symbol"
    (def discard (setup (ui/symbol 'foo)))
    % := 'foo
    (uit/focus @it)
    (uit/set-value! @it 'bar)
    % := 'bar
    (uit/set-value! @it "123")            ; nothing, not a keyword
    (uit/set-value! @it "quux")
    % := 'quux
    (uit/set-value! @it "foo/bar")
    % := 'foo/bar
    (uit/blur @it)
    % := 'foo
    (discard)
    )

  (tests "ui/uuid"
    (def initial-uuid #uuid "a87f641b-574d-4cdd-93d2-2fb5332b697e")
    (def discard (setup (ui/uuid initial-uuid)))
    % := initial-uuid
    (def another #uuid "073d5061-b77d-48f3-a0eb-b1cb59cf5ea1")
    (uit/focus @it)
    (uit/set-value! @it another)
    % := another
    (uit/set-value! @it "foobar")         ; nothing, not a uuid
    (def and-another #uuid "0617f36f-631a-4796-bb55-fafc0d99ff19")
    (uit/set-value! @it and-another)
    % := and-another
    (uit/blur @it)
    % := initial-uuid
    (discard)
    )

  (tests "ui/edn"
    (def discard (setup (ui/edn {:hi :there})))
    % := {:hi :there}
    (uit/focus @it)
    (uit/set-value! @it :new-value)
    % := :new-value
    (uit/set-value! @it ":foo/")          ; nothing, not valid EDN
    (uit/set-value! @it (pr-str ":foo"))
    % := ":foo"
    (uit/blur @it)
    % := {:hi :there}
    (discard)
    )

  (tests "ui/date"
    (def discard (setup (ui/date "2022-11-30")))
    % := "2022-11-30"
    (uit/focus @it)
    (uit/set-value! @it "2022-01-01")
    % := "2022-01-01"
    (uit/set-value! @it "nonsense")       ; nothing, not valid Date string
    (uit/set-value! @it "2000-01-01")
    % := "2000-01-01"
    (uit/blur @it)
    % := "2022-11-30"
    (discard)
    )


  )
