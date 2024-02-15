(ns hyperfiddle.electric.impl.cljs-analyzer-test
  (:require [clojure.test :as t]
            [hyperfiddle.electric.impl.cljs-analyzer :as ana]))

(t/deftest all
  (let [a (ana/analyze-ns 'hyperfiddle.electric.impl.cljs-file-to-analyze)]
    (t/is (nil? (ana/find-var 'non a)))
    (t/is (nil? (ana/find-var 'first a)))
    (t/are [x] (some? (ana/find-var x a))
      'foo
      'bar
      'baz
      'an-fn
      'behind-require
      'str
      'behind-alias
      'behind-require-macros
      'behind-require-macro-alias
      'behind-required-refer
      'behind-required-rename
      'behind-require-macro-refer
      'behind-require-macro-rename
      'behind-include-macros
      'behind-refer-macros
      'behind-use
      'behind-use-renamed
      'behind-use-macro
      'behind-use-macro-renamed
      'nxt
      ;; 'refnonmacro                        ; TODO
      )))
