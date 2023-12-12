(ns hyperfiddle.electric-compiler-test
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]
            [hyperfiddle.electric.impl.lang-de :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]))

;; tests that turn electric code into clojure code
;; basically no IR, we emit clojure code directly

;; (e/defn Foo [x]
;;   (inc x))                              ; hyperfiddle.electric-compiler-test/Foo:10:3
;; (r/apply (r/static inc) (r/local 'x))

;; lang/source-map with same signature as lang/compile
;; it returns the same structure as r/defs
;; but instead of the definitions it contains the metadata
#_(defn r/apply [..]
  (try (apply f args)
       (catch Throwable e (find-source-map-info path))))

(tests
  ;; (defn lang/compile [env form]
  (lang/compile-client {} 1)
  ;; r/defs takes & flows with an implicit context (managed in runtime, thread-local or such)
  ;; context - path of the node you're constructing in the call stack
  := `(r/defs (r/static 1))


  )
