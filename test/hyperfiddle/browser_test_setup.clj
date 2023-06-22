(ns hyperfiddle.browser-test-setup
  (:require [cljs.analyzer :as ana]))

;; makes sure CI fails when there are undeclared var warnings
;; useful to test goog requires
(defn blow-up-tests-on-warnings {:shadow.build/stage :compile-prepare} [build-state]
  (defmethod ana/error-message :undeclared-var [_warning-type info]
    (throw (ex-info (str "undeclared var: " (:prefix info) "/" (:suffix info)) info)))
  build-state)
