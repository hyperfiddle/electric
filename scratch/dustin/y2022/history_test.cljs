(ns dustin.y2022.history-test
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  (:require-macros dustin.scratch))


(comment
  (js/alert "hello")
  js/window.history.back
  (p/run (Router. 1)))

(comment
  ; https://google.github.io/closure-library/api/goog.History.html
  ; problem - cannot unattach goog.history in order to dispose
  (def h (new goog.History)) ; mutate global window state
  (.getValue h) := _ "/"

  (.pushState h "/foo")
  (.getValue h) := _ "/foo"

  ; possible problem: to rerun the page, need to refresh
  ; possib

  ; if we have a namespace which such kind of tests
  ; the ns needs to be excluded from hot code reloading
  )

(comment
  (with (p/run
          (binding [dom/node (.createElement js/document "div")]
            #_(dom/text "hello world")
            #_(println (.. dom/node -innerText))
            ))))

(tests
  (def dispose
    (p/run
      (binding [dom/node (.createElement js/document "div")]
        (dom/text "hello world")
        (println (.. dom/node -innerText))
        (tap (.. dom/node -innerText)))))
  % := "hello world"
  (dispose))

