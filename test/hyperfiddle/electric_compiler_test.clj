(ns hyperfiddle.electric-compiler-test
  (:require [hyperfiddle.electic :as-alias e]
            [hyperfiddle.rcf :as rcf :refer [tests]]
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

  ;; transfer
  (lang/compile-client {}
    `(e/client "Hello world")) :=
  `(r/defs
     (r/static "Hello world"))
  (lang/compile-client {}
    `(e/client
       (let [x "Hello world"
             y "Hello world"]
         [x y]))) :=
  `(r/defs
     (r/ap (r/static vector) (r/local 1) (r/local 2))
     (r/static "Hello world")
     (r/static "Hello world"))

  (lang/compile-server {}
    `(e/client "Hello world")) :=
  `(r/defs
     (r/remote 0))                                           ;; 0 refers to client's r/defs

  ;; function application
  (lang/compile-client {}
    `(e/client (prn "Hello world"))) :=
  `(r/defs
     (r/ap (r/static prn) (r/static "Hello world")))

  ;; lexical scope
  (lang/compile-client {}
    `(e/client (let [a :foo] [a a]))) :=
  `(r/defs
     (r/ap (r/static vector) (r/local 1) (r/local 1))
     (r/static :foo))

  (lang/compile-client {}
    `(e/client
       (let [a (e/server :foo)]
         (e/server (prn a))))) :=
  `(r/defs
     (r/remote 0))
  (lang/compile-server {}
    `(e/client
       (let [a (e/server :foo)]
         (e/server (prn a))))) :=
  `(r/defs
     (r/ap (r/static prn) (r/local 1))
     (r/static :foo))

  ;; join (e/watch !x)
  ;; (i/fixed continuous-flow) -> incremental sequence of 1 element
  (lang/compile-client {}
    `(e/client (e/join (i/fixed (m/watch !x))))) :=
  `(r/defs
     (r/join (r/ap (r/static i/fixed) (r/ap (r/static m/watch) (r/static !x)))))

  ;; pure (get the incseq of an expression) (e/pure (e/join x)) is (e/join (e/pure x)) is x
  (lang/compile-client {}
    `(e/client (e/pure :foo))) :=
  `(r/defs
     (r/pure (r/static :foo)))

  ;; ctor (e/fn [] foo) -> (e/ctor foo) (previously ::c/closure)
  (lang/compile-client {}
    `(e/client (e/ctor :foo))) :=
  `(r/defs
     (r/ctor 1)
     (r/static :foo))

  ;; call (aka new, but with no argument and only for ctors)
  (lang/compile-client {}
    `(e/client (e/call (e/ctor :foo)))) :=
  `(r/defs
     (r/call (r/ctor 1))
     (r/static :foo))

  ;; lexical closure
  (lang/compile-client {}
    `(e/client
       (let [a :foo]
         (e/call (e/ctor a))))) :=
  `(r/defs
     (r/call (r/ctor 1 (r/local 2)))
     (r/free 0)
     (r/static :foo))

  (lang/compile-client {}
    `(e/client
       (let [a :foo]
         (e/call (e/ctor (e/ctor a)))))) :=
  `(r/defs
     (r/call (r/ctor 1 (r/local 3)))
     (r/ctor 2 (r/free 0))
     (r/free 0)
     (r/static :foo))

  ;; conditionals
  (lang/compile-client {}
    `(e/client (case :x nil :y :z))) :=
  `(r/defs
     (r/call (r/ap (r/ap (r/static hash-map)
                     (r/static nil) (r/ctor 1))
               (r/static :x) (r/ctor 2)))
     (r/static :y)
     (r/static :z))

  ;; var
  (e/def x)
  (lang/compile-client {} `(e/client (var x))) :=
  `(r/defs (r/var (quote x)))


  )
