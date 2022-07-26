(ns hyperfiddle.photon-dom-test
  (:require [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests % !]]
            [clojure.test :as t]))

(def CONTAINER (dom/by-id "test-bench"))
(defn text-content [e] (.-textContent e))

(t/use-fixtures :each {:before (fn [] (set! (.-innerHTML CONTAINER) ""))})

(tests
  (text-content CONTAINER) := ""
  (let [dispose (p/run (binding [dom/node CONTAINER]
                          (dom/text "hello")
                          (! (text-content CONTAINER))))]
    % := "hello"
    (dispose)
    (text-content CONTAINER) := ""))

(tests
  (def !x (atom true))
  (p/run
    (binding [dom/node CONTAINER]
      (dom/text "a")
      (if (p/watch !x)
        (dom/text "b")
        (dom/text "c"))
      (dom/text "d"))
    (! (text-content CONTAINER)))

  % := "abd"
  (swap! !x not)
  % := "acd"
  (swap! !x not)
  % := "abd"
  )

(tests
  (def !xs (atom ["b" "c"]))
  (p/run
    (binding [dom/node CONTAINER]
      (dom/text "a")
      (p/for [x ~(m/watch !xs)]
        (dom/text x))
      (dom/text "d"))
    (! (text-content CONTAINER)))

  % := "abcd"
  (swap! !xs reverse)
  % := "acbd"
  (swap! !xs reverse)
  % := "abcd"
  )


(comment
  ;; CLJ
  (alter-var-root #'hyperfiddle.rcf/*generate-tests* (constantly true))
  (shadow.cljs.devtools.api/repl :devkit)
  ;; CLJS
  (t/run-tests)
  )
