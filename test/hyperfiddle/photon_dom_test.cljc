(ns hyperfiddle.photon-dom-test
  (:require [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests % !]]))

(def body (.-body js/document))
(defn text-content [e]
  (.-textContent e))

(tests
  (def !x (atom false))
  (p/run
    (binding [dom/parent body]
      (dom/text "a")
      (if ~(m/watch !x)
        (dom/text "b")
        (dom/text "c"))
      (dom/text "d"))
    (! (text-content body)))

  % := "abd"
  (swap! !x not)
  % := "acd"
  (swap! !x not)
  % := "abd"
  )