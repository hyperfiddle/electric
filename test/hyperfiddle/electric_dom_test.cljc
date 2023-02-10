(ns hyperfiddle.electric-dom-test
  (:require [missionary.core :as m]
            [hyperfiddle.electric :as p]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.rcf :refer [tests % tap]]
            [clojure.test :as t]))

(def root js/document.body)

(t/use-fixtures :each {:before (fn [] (set! (.-innerHTML root) ""))})

(tests
  "dom text node mount and unmount"
  (.-textContent root) := ""
  (let [dispose (p/run (binding [dom/node root]
                         (dom/text "hello")
                         (tap (.-textContent root))))]
    % := "hello"
    (dispose)
    (.-textContent root) := ""))

(tests
  "switch on dom effect"
  (def !x (atom true))
  (p/run
    (binding [dom/node root]
      (dom/text "a")
      (if (p/watch !x)
        (dom/text "b")
        (dom/text "c"))
      (dom/text "d"))
    (tap (.-textContent root)))

  % := "abd"
  (swap! !x not)
  % := "acd"
  (swap! !x not)
  % := "abd"
  )

(tests
  "dynamic dom ordering"
  (def !xs (atom ["b" "c"]))
  (p/run
    (binding [dom/node root]
      (dom/text "a")
      (p/for [x ~(m/watch !xs)]
        (dom/text x))
      (dom/text "d"))
    (tap (.-textContent root)))

  % := "abcd"
  (swap! !xs reverse)
  % := "acbd"
  (swap! !xs reverse)
  % := "abcd"
  )


(comment
  ;; CLJ
  (alter-var-root #'hyperfiddle.rcf/*generate-tests* (constantly true))
  (shadow.cljs.devtools.api/repl :dev)
  ;; CLJS
  (t/run-tests)
  )
