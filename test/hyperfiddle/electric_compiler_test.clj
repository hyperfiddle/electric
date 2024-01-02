(ns hyperfiddle.electric-compiler-test
  (:require [hyperfiddle.electic :as-alias e]
            [hyperfiddle.incseq :as i]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [hyperfiddle.electric.impl.lang-de :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.electric-local-def-de :as l]
            [hyperfiddle.incseq :as i]
            [missionary.core :as m]))

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
  (l/compile-client 1) :=  `(r/peer (lang/r-defs (lang/r-static 1)) [] 0)
  (l/compile-server 1) :=  `(r/peer (lang/r-defs (lang/r-static 1)) [] 0)

  (l/compile-server (prn "hello world")) := `(r/peer (lang/r-defs (lang/r-ap (lang/r-static ~'prn) (lang/r-static "hello world"))) [] 0)
  (l/compile-client (let [x "Hello world", y "Hello world"] [x y]))
  := `(r/peer (lang/r-defs
                (lang/r-static "Hello world")
                (lang/r-static "Hello world")
                (lang/r-ap (lang/r-static vector) (lang/r-local 0) (lang/r-local 1))) [] 2)

  (l/compile-client (concat (let [x 1] [x x]) (let [y 2] [y y])))
  := `(r/peer (lang/r-defs
                (lang/r-static 1)
                (lang/r-static 2)
                (lang/r-ap (lang/r-static ~'concat)
                  (lang/r-ap (lang/r-static vector) (lang/r-local 0) (lang/r-local 0))
                  (lang/r-ap (lang/r-static vector) (lang/r-local 1) (lang/r-local 1)))) [] 2)

  (l/compile-client (i/fixed (m/watch (atom 0))))
  := `(r/peer (lang/r-defs
                (lang/r-ap (lang/r-static ~'i/fixed)
                  (lang/r-ap (lang/r-static ~'m/watch)
                    (lang/r-ap (lang/r-static ~'atom)
                      (lang/r-static 0))))) [] 0)

  (l/compile-client (::lang/ctor :foo))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-ctor [] 0))
        [] 1)

  (l/compile-client (let [a 1] (::lang/ctor a)))
  := `(r/peer
        (lang/r-defs
          (lang/r-static 1)
          (lang/r-free 0)
          (lang/r-ctor [] 1 (lang/r-local 0)))
        [] 2)

  (l/compile-client (let [a 1] (::lang/ctor (let [a 2] a))))
  := `(r/peer
        (lang/r-defs
          (lang/r-static 2)
          (lang/r-ctor [] 0))
        [] 1)

  (l/compile-client (let [a 1] (::lang/ctor (::lang/ctor a))))
  := `(r/peer
        (lang/r-defs
          (lang/r-static 1)
          (lang/r-free 0)
          (lang/r-ctor [] 1 (lang/r-free 0))
          (lang/r-ctor [] 2 (lang/r-local 0)))
        [] 3)

  (l/compile-client (let [a 1] (::lang/ctor [a (let [a 2] (::lang/ctor a))])))
  := `(r/peer
        (lang/r-defs
          (lang/r-static 1)
          (lang/r-static 2)
          (lang/r-free 0)
          (lang/r-ap (lang/r-static vector)
            (lang/r-free 0)
            (lang/r-ctor [] 2 (lang/r-local 1)))
          (lang/r-ctor [] 3 (lang/r-local 0)))
        [] 4)

  (l/compile-client (let [a 1] (::lang/ctor (::lang/ctor (::lang/ctor a)))))
  := `(r/peer
        (lang/r-defs
          (lang/r-static 1)
          (lang/r-free 0)
          (lang/r-ctor [] 1 (lang/r-free 0))
          (lang/r-ctor [] 2 (lang/r-free 0))
          (lang/r-ctor [] 3 (lang/r-local 0)))
        [] 4)

  (l/compile-client (let [a 1, b 2] (::lang/ctor [a (::lang/ctor b)])))
  := `(r/peer
        (lang/r-defs
          (lang/r-static 1)
          (lang/r-static 2)
          (lang/r-free 0)
          (lang/r-ap (lang/r-static clojure.core/vector)
            (lang/r-free 0)
            (lang/r-ctor [] 2 (lang/r-free 1)))
          (lang/r-ctor [] 3 (lang/r-local 0) (lang/r-local 1)))
        []
        4)

  (l/compile-client (let [a 1, b 2] (::lang/ctor [b (::lang/ctor a)])))
  := `(r/peer
        (lang/r-defs
          (lang/r-static 2)
          (lang/r-static 1)
          (lang/r-free 0)
          (lang/r-ap (lang/r-static clojure.core/vector)
            (lang/r-free 1)
            (lang/r-ctor [] 2 (lang/r-free 0)))
          (lang/r-ctor [] 3 (lang/r-local 1) (lang/r-local 0)))
        []
        4)

  (l/compile-client (let [x (::lang/ctor :foo)] x))
  := `(r/peer
        (lang/r-defs
          (lang/r-ctor [] 1)
          (lang/r-static :foo)
          (lang/r-local 0))
        [] 2)

  (l/compile-client (let [x (::lang/ctor :foo), y x] (::lang/call y)))
  := `(r/peer
        (lang/r-defs
          (lang/r-local 1)
          (lang/r-ctor [] 2)
          (lang/r-static :foo)
          (lang/r-call 0))
        [0] 3)

  (l/compile-client (::lang/call (::lang/ctor :foo)))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-ctor [] 0)
          (lang/r-call 0))
        [1] 2)

  (l/compile-client (vector 1 (::lang/call (::lang/ctor :foo))))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-ctor [] 0)
          (lang/r-ap (lang/r-static ~'vector)
            (lang/r-static 1)
            (lang/r-call 0)))
        [1] 2)

  (l/compile-client (let [x (::lang/ctor :foo)] [(::lang/call x) (::lang/call x)]))
  := `(r/peer
        (lang/r-defs
          (lang/r-ctor [] 1)
          (lang/r-static :foo)
          (lang/r-ap (lang/r-static clojure.core/vector)
            (lang/r-call 0)
            (lang/r-call 1)))
        [0 0]
        2)
  := `(r/peer
        (lang/r-defs
          (lang/r-ctor [] 1)
          (lang/r-static :foo)
          (lang/r-local 0)
          (lang/r-local 0)
          (lang/r-ap (lang/r-static clojure.core/vector)
            (lang/r-call 0)
            (lang/r-call 1)))
        [2 3] 4)

  (l/compile-client [(::lang/call (::lang/ctor :foo)) (::lang/call (::lang/ctor :bar))])
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-ctor [] 0)
          (lang/r-static :bar)
          (lang/r-ctor [] 2)
          (lang/r-ap (lang/r-static clojure.core/vector)
            (lang/r-call 0)
            (lang/r-call 1)))
        [1 3] 4)

  (l/compile-client (let [a :foo] (::lang/call (::lang/ctor (::lang/ctor a)))))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-free 0)
          (lang/r-ctor [] 1 (lang/r-free 0))
          (lang/r-ctor [] 2 (lang/r-local 0))
          (lang/r-call 0))
        [3] 4)
  )

(comment
  ;; (defn lang/compile [env form]
  (l/compile-client 1)
  ;; r/defs takes & flows with an implicit context (managed in runtime, thread-local or such)
  ;; context - path of the node you're constructing in the call stack
  := `(r/peer
        (lang/r-defs
          (lang/r-static 1))
        [] 0)

  ;; transfer
  (l/compile-client (e/client "Hello world")) :=
  `(r/peer
     (lang/r-defs
       (lang/r-static "Hello world"))
     [] 0)
  (l/compile-client (e/client
                      (let [x "Hello world"
                            y "Hello world"]
                        [x y])))
  := `(r/peer
        (lang/r-defs
          (lang/r-static "Hello world")
          (lang/r-static "Hello world")
          (lang/r-ap (lang/r-static vector) (lang/r-local 0) (lang/r-local 1)))
        [] 2)

  (l/compile-server (e/client "Hello world"))
  := `(r/peer
        (lang/r-defs
          (lang/r-remote 0)) ;; 0 refers to client's r/defs
        [] 0)

  ;; function application
  (l/compile-client (e/client (prn "Hello world")))
  := `(r/peer
        (lang/r-defs
          (lang/r-ap (lang/r-static prn) (lang/r-static "Hello world")))
        [] 0)

  ;; lexical scope
  (l/compile-client (e/client (let [a :foo] [a a])))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-ap (lang/r-static vector) (lang/r-local 1) (lang/r-local 1)))
        [] 1)

  (l/compile-client (e/client
                      (let [a (e/server :foo)]
                        (e/server (prn a)))))
  := `(r/peer
        (lang/r-defs
          (lang/r-remote 0))
        [] 0)
  (l/compile-server (e/client
                      (let [a (e/server :foo)]
                        (e/server (prn a)))))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-ap (lang/r-static prn) (lang/r-local 0)))
        [] 1)

  ;; join (e/watch !x)
  ;; (i/fixed continuous-flow) -> incremental sequence of 1 element
  (l/compile-client `(e/client (e/join (i/fixed (m/watch !x)))))
  := `(r/peer
        (lang/r-defs
          (lang/r-join (lang/r-ap (lang/r-static i/fixed)
                       (lang/r-ap (lang/r-static m/watch)
                         (lang/r-static !x)))))
        [] 0)

  ;; pure (get the incseq of an expression) (e/pure (e/join x)) is (e/join (e/pure x)) is x
  (l/compile-client (e/client (e/pure :foo)))
  := `(r/peer
        (lang/r-defs
          (lang/r-static (lang/r-static :foo)))
        [] 0)

  ;; ctor (e/fn [] foo) -> (e/ctor foo) (previously ::c/closure)
  (l/compile-client (e/client (e/ctor :foo)))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-ctor [] 0))
        [] 1)

  ;; call (aka new, but with no argument and only for ctors)
  (l/compile-client (e/client (e/call (e/ctor :foo))))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-ctor [] 0)
          (lang/r-call 0))
        [1] 2)

  ;; lexical closure
  (l/compile-client (e/client
                      (let [a :foo]
                        (e/call (e/ctor a)))))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-free 0)
          (lang/r-ctor [] 1 (lang/r-local 0))
          (lang/r-call 0))
        [2] 3)

  (l/compile-client (e/client
                      (let [a :foo]
                        (e/call (e/ctor (e/ctor a))))))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-free 0)
          (lang/r-ctor [] 1 (lang/r-free 0))
          (lang/r-ctor [] 2 (lang/r-local 0))
          (lang/r-call 0))
        [3] 4)

  ;; conditionals
  (l/compile-client `(e/client (case :x nil :y :z)))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :y)
          (lang/r-static :z)
          (lang/r-ap (lang/r-ap (lang/r-static hash-map)
                     (lang/r-static nil) (lang/r-ctor [] 0))
            (lang/r-static :x) (lang/r-ctor [] 1))
          (lang/r-call 0))
        [2] 3)

  ;; var
  (e/def x)
  (l/compile-client (e/client (var x)))
  := `(r/peer
        (lang/r-defs
          (lang/r-var x))
        [] 0)

  (l/compile-client (let [a :foo, b :bar, c :baz]
                      [(e/ctor [a b]) (e/ctor [b c])]))
  := `(r/peer
        (lang/r-defs
          (lang/r-static :foo)
          (lang/r-static :bar)
          (lang/r-static :baz)
          (lang/r-ap (lang/r-static vector)
            (lang/r-free 0) (lang/r-free 1))
          (lang/r-ap (lang/r-static vector)
            (lang/r-free 0) (lang/r-free 1))
          (lang/r-ap (lang/r-static vector)
            (lang/r-ctor 3 (lang/r-local 0) (lang/r-local 1))
            (lang/r-ctor 4 (lang/r-local 1) (lang/r-local 2))))
        [] 5)

  (l/compile-client (new (e/fn Foo [] (Foo.))))
  := `(r/peer
        (fn [tier id]
          (case id
            0 (r/ctor-free (r/tier-ctor tier) 0)
            1 (let [free (object-array 1)
                    ctor (r/peer-ctor (r/tier-peer tier) [] 0 free)]
                (aset free 0 (r/pure ctor))
                (r/pure ctor))
            2 (i/latest-concat (r/tier-slot tier 0))))
        [1] 2)

  (l/compile-client (e/letfn [(Foo [] (Bar.))
                              (Bar [] (Foo.))]
                      (Foo.)))
  := `(r/peer
        (fn [tier id]
          (case id
            0 (r/ctor-free (r/tier-ctor tier) 1)
            1 (r/ctor-free (r/tier-ctor tier) 0)
            2 (let [Foo-free (object-array 2)
                    Foo-ctor (r/peer-ctor (r/tier-peer tier) [] 0 Foo-free)
                    Bar-free (object-array 2)
                    Bar-ctor (r/peer-ctor (r/tier-peer tier) [] 1 Bar-free)]
                (aset Foo-free 0 (r/pure Foo-ctor))
                (aset Foo-free 1 (r/pure Bar-ctor))
                (aset Bar-free 0 (r/pure Foo-ctor))
                (aset Bar-free 1 (r/pure Bar-ctor))
                (r/pure {:Foo Foo-ctor :Bar Bar-ctor}))
            3 (i/latest-product :Foo (r/tier-local tier 0))
            4 (i/latest-concat (r/tier-slot tier 0))))
        [3] 4)

  (l/compile-client (let [a :foo, b :bar, c :baz]
                      [(e/ctor [a b]) (e/ctor [b c])]))
  := `(lang/r-defs
        (lang/r-ap (lang/r-static vector)
          (lang/r-ctor 4 (lang/r-local 1) (lang/r-local 2))
          (lang/r-ctor 5 (lang/r-local 2) (lang/r-local 3)))
        (lang/r-static :foo)
        (lang/r-static :bar)
        (lang/r-static :baz)
        (lang/r-ap (lang/r-static vector) (lang/r-free 0) (lang/r-free 1))
        (lang/r-ap (lang/r-static vector) (lang/r-free 0) (lang/r-free 1)))

  #_(e/defn Foo [])
  (l/compile-client `(e/ctor (e/call Foo))) :=
  `(r/peer
     (lang/r-defs
       (lang/r-lookup Foo)
       (lang/r-call 0)
       (lang/r-ctor [0] 1))
     [] 2)

  (l/compile-client `e/tier) :=
  `(r/peer
     (fn [tier id]
       (case id
         0 (r/pure tier)))
     [] 0)

  )
