(ns hyperfiddle.js-calls-test
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            ["./js_calls_test" :as call-test]
            [hyperfiddle.electric.impl.runtime :as r]))

(call-test/install) ; required for later tests

(tests
  "js scoped call in cljs"
  (call-test/scope.fn) := "value"
  (.fn call-test/scope) := "value"
  (js/hyperfiddle.js_calls_test.scope.fn) := "value" ; requires `(call-test/install)`
  (let [fn (.-fn call-test/scope)]
    (undefined? (fn)) := true                       ; fn lost its `this` context
    ((r/bound-js-fn call-test/scope fn)) := "value" ; re-set `this` context to `scope`
    ))

(tests
  "js scoped call in electric"
  (with (e/run
          (tap (call-test/scope.fn)) ; direct access
          (tap (.fn call-test/scope)) ; two-step access
          (tap (js/hyperfiddle.js_calls_test.scope.fn)) ; global access, requires `(call-test/install)`
          (let [fn (.-fn call-test/scope)]
            (tap (undefined? (fn)))))
    % := "value"
    % := "value"
    % := "value"
    % := true
    ))

