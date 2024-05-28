(ns hyperfiddle.js-calls-test-de
  (:require [hyperfiddle.electric-local-def-de :as l]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.electric.impl.lang-de2 :as lang]
            ["./js_calls_test_de" :as call-test]))

;;; Goal: confirm Electric and CLJS have the same js function call semantics.

(call-test/install) ; required for later tests

;;; The two tests blocks should be identical in intent and result.

;; CLJS
(tests
  "js scoped call in cljs"
  call-test/scope.fn := call-test/scope.fn
  (call-test/scope.fn) := "value"
  (.fn call-test/scope) := "value"
  (js/hyperfiddle.js_calls_test_de.scope.fn) := "value" ; requires `(call-test/install)`
  (let [fn (.-fn call-test/scope)]
    (undefined? (fn)) := true               ; fn lost its `this` context
    ((.bind fn call-test/scope)) := "value" ; re-set `this` context to `scope`
    ))

;; Electric
(tests
  "js scoped call in electric"
  (with ((l/single {}
           (let [fn (.-fn call-test/scope)]
             (tap [call-test/scope.fn
                   (call-test/scope.fn)                       ; direct access
                   (.fn call-test/scope)                      ; two-step access
                   (js/hyperfiddle.js_calls_test_de.scope.fn) ; global access, requires `(call-test/install)`
                   (undefined? (fn))
                   ((.bind fn call-test/scope))]))) tap tap)
    % := [call-test/scope.fn "value" "value" "value" true "value"]))

