(ns hyperfiddle.ui.codemirror2-test
  (:require [hyperfiddle.ui.test :as uit]
            [hyperfiddle.rcf :as rcf :refer [% tap tests with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui.codemirror2 :as cm]))

(tests "cm/string"
  (def discard (p/run (binding [dom/node (dom/by-id "root")]
                        (tap (new cm/string "hi")))))
  ;; (def line (.querySelector js/document ".cm-line"))
  ;; (def content (.querySelector js/document ".cm-line"))
  ;; (.dispatchEvent line (js/Event. "mousedown"))
  ;; (.dispatchEvent content (js/Event. "mousedown"))
  ;; (uit/focus line)
  ;; (uit/focus content)
  ;; (set! (.-innerText line) "there")

  % := "hi"
  ;; TODO I see this works when trying out in the REPL and interacting with the browser manually
  ;; but I can't seem to trigger the user-like typing behavior from the test.
  ;; Exposing the EditorView doesn't help because the `on-change` handler
  ;; checks if the action was a user action.
  ;; % := "there"
  ;; % := "buddy"
  (discard)
  )
