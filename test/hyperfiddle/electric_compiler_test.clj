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
  := `(r/peer
        (lang/defs
          (lang/static 1))
        [] 0)

  ;; transfer
  (lang/compile-client {}
    `(e/client "Hello world")) :=
  `(r/peer
     (lang/defs
       (lang/static "Hello world"))
     [] 0)
  (lang/compile-client {}
    `(e/client
       (let [x "Hello world"
             y "Hello world"]
         [x y]))) :=
  `(r/peer
     (lang/defs
       (lang/static "Hello world")
       (lang/static "Hello world")
       (lang/ap (lang/static vector) (lang/local 1) (lang/local 2)))
     [] 2)

  (lang/compile-server {}
    `(e/client "Hello world")) :=
  `(r/peer
     (lang/defs
       (lang/remote 0))  ;; 0 refers to client's r/defs
     [] 0)

  ;; function application
  (lang/compile-client {}
    `(e/client (prn "Hello world"))) :=
  `(r/peer
     (lang/defs
       (lang/ap (lang/static prn) (lang/static "Hello world")))
     [] 0)

  ;; lexical scope
  (lang/compile-client {}
    `(e/client (let [a :foo] [a a]))) :=
  `(r/peer
     (lang/defs
       (lang/static :foo)
       (lang/ap (lang/static vector) (lang/local 1) (lang/local 1)))
     [] 1)

  (lang/compile-client {}
    `(e/client
       (let [a (e/server :foo)]
         (e/server (prn a))))) :=
  `(r/peer
     (lang/defs
       (lang/remote 0))
     [] 0)
  (lang/compile-server {}
    `(e/client
       (let [a (e/server :foo)]
         (e/server (prn a))))) :=
  `(r/peer
     (lang/defs
       (lang/static :foo)
       (lang/ap (lang/static prn) (lang/local 0)))
     [] 1)

  ;; join (e/watch !x)
  ;; (i/fixed continuous-flow) -> incremental sequence of 1 element
  (lang/compile-client {}
    `(e/client (e/join (i/fixed (m/watch !x))))) :=
  `(r/peer
     (lang/defs
       (lang/join (lang/ap (lang/static i/fixed)
                    (lang/ap (lang/static m/watch)
                      (lang/static !x)))))
     [] 0)

  ;; pure (get the incseq of an expression) (e/pure (e/join x)) is (e/join (e/pure x)) is x
  (lang/compile-client {}
    `(e/client (e/pure :foo))) :=
  `(r/peer
     (lang/defs
       (lang/static (lang/static :foo)))
     [] 0)

  ;; ctor (e/fn [] foo) -> (e/ctor foo) (previously ::c/closure)
  (lang/compile-client {}
    `(e/client (e/ctor :foo))) :=
  `(r/peer
     (lang/defs
       (lang/static :foo)
       (lang/ctor [] 0))
     [] 1)

  ;; call (aka new, but with no argument and only for ctors)
  (lang/compile-client {}
    `(e/client (e/call (e/ctor :foo)))) :=
  `(r/peer
     (lang/defs
       (lang/static :foo)
       (lang/ctor [] 0)
       (lang/call 0))
     [1] 2)

  ;; lexical closure
  (lang/compile-client {}
    `(e/client
       (let [a :foo]
         (e/call (e/ctor a))))) :=
  `(r/peer
     (lang/defs
       (lang/static :foo)
       (lang/free 0)
       (lang/ctor [] 1 (lang/local 0))
       (lang/call 0))
     [2] 3)

  (lang/compile-client {}
    `(e/client
       (let [a :foo]
         (e/call (e/ctor (e/ctor a)))))) :=
  `(r/peer
     (lang/defs
       (lang/static :foo)
       (lang/free 0)
       (lang/ctor [] 1 (lang/free 0))
       (lang/ctor [] 2 (lang/local 0))
       (lang/call 0))
     [3] 4)

  ;; conditionals
  (lang/compile-client {}
    `(e/client (case :x nil :y :z))) :=
  `(r/peer
     (lang/defs
       (lang/static :y)
       (lang/static :z)
       (lang/ap (lang/ap (lang/static hash-map)
                  (lang/static nil) (lang/ctor [] 0))
         (lang/static :x) (lang/ctor [] 1))
       (lang/call 0))
     [2] 3)

  ;; var
  (e/def x)
  (lang/compile-client {} `(e/client (var x))) :=
  `(r/peer
     (lang/defs
       (lang/var x))
     [] 0)

  (lang/compile-client {}
    `(let [a :foo, b :bar, c :baz]
       [(e/ctor [a b]) (e/ctor [b c])])) :=
  `(r/peer
     (lang/defs
       (lang/static :foo)
       (lang/static :bar)
       (lang/static :baz)
       (lang/ap (lang/static vector)
         (lang/free 0) (lang/free 1))
       (lang/ap (lang/static vector)
         (lang/free 0) (lang/free 1))
       (lang/ap (lang/static vector)
         (lang/ctor 3 (lang/local 0) (lang/local 1))
         (lang/ctor 4 (lang/local 1) (lang/local 2))))
     [] 5)

  )
