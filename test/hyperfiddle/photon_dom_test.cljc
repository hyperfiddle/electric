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
    (binding [dom/node body]
      (dom/text "a")
      (if (new (m/watch !x))
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

(tests
  (def !xs (atom ["b" "c"]))
  (p/run
    (binding [dom/node body]
      (dom/text "a")
      (p/for [x ~(m/watch !xs)]
        (dom/text x))
      (dom/text "d"))
    (! (text-content body)))

  % := "abcd"
  (swap! !xs reverse)
  % := "acbd"
  (swap! !xs reverse)
  % := "abcd"
  )