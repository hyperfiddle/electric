(ns dustin.y2022.rcf2-fun
  (:require [clojure.test]
            [hyperfiddle.rcf :as rcf :refer [tests tap %]]))

(hyperfiddle.rcf/enable!)
#?(:clj (defmethod clojure.test/assert-expr `= [msg form] (rcf/assert-= nil msg form)))
;(defmethod rcf/rewrite-sigil-hook '=> [_] :=)

(tests
  "works"
  1 := 1
  1 :hyperfiddle.rcf/= 1
  1 hyperfiddle.rcf/= 1
  1 clojure.core/= 1)

(tests
  "doesn't work"
  1 rcf/= 1
  1 = 1)

(tests
  (assert false) hyperfiddle.rcf/thrown? AssertionError
  (assert false) thrown? AssertionError
  nil)

(defn foo [x]
  (assert (= x 1))
  (tests x := 1)
  (doto x (tests := 1))
  )

(foo 1)

(defn bar [x] x := 1)
;(tests (bar 2))

(tests (clojure.test/is (::rcf/= 1 1)))
(tests (clojure.test/is (= 1 1)))
(tests (::rcf/= 1 1))

(tests (throw (ex-info "hello" {})) thrown? clojure.lang.ExceptionInfo 1)
