(ns hyperfiddle.electric.impl.compiler-test
  (:require [hyperfiddle.electric-de :as e]
            [hyperfiddle.incseq :as i]
            #?(:clj [contrib.triple-store :as ts])
            #?(:clj [hyperfiddle.electric.impl.lang-de2 :as lang])
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.electric-local-def-de :as l]
            #?(:clj [hyperfiddle.electric.impl.compiler-test-clj :refer [cannot-be-unsited]]
               :cljs [hyperfiddle.electric.impl.compiler-test-cljs :refer [cannot-be-unsited]])
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [hyperfiddle.electric.impl.expand-require-referred :as ref :refer [referred referred-fn]]
            #?(:clj [contrib.test-match :as tm])
            [fipp.edn]
            [missionary.core :as m])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

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

(defn ->code [v] `(fn tm/_ ([tm/_] (case tm/_ ~@(interleave (range) v)))))

(defmacro match [code matcher]
  `(let [ret# ~code, matcher# (->code ~matcher) match# (tm/test-match ret# matcher#)]
     ret# := match#
     (when (not= ret# match#) (fipp.edn/pprint match#))
     match#))

;; no `:=`, these just need to compile
(l/test-compile ::Main (lang/->cljs-env) referred-fn)
(l/test-compile ::Main (lang/->cljs-env) ref/referred-fn)
(l/test-compile ::Main (lang/->cljs-env) hyperfiddle.electric.impl.expand-require-referred/referred-fn)

(tests "test-simplest"
  (match (l/test-compile ::Main 1)
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure 1)))])
  (match (l/test-compile ::Main (prn "Hello world"))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/prn (r/pure prn)) (r/pure "Hello world"))))])
  (match (l/test-compile ::Main (prn (e/client 1)))
    `[(r/cdef 0 [:client] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/ap (r/lookup ~'frame :clojure.core/prn (r/pure clojure.core/prn))
            (r/node ~'frame 0))))]))

(tests "test-join"
  (match (l/test-compile ::Main {'!x (atom 0)} (::lang/join (i/fixed (m/watch !x))))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/join
            (r/ap (r/lookup ~'frame ::i/fixed (r/pure i/fixed))
              (r/ap (r/lookup ~'frame ::m/watch (r/pure m/watch))
                (r/pure ~'!x))))))]))

(tests "test-siting"
  (match (l/test-compile ::Main (::lang/site :client "Hello world"))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/pure "Hello world")))])

  (match (l/test-compile ::Main (::lang/site :client (prn "Hello world")))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/prn) (r/pure "Hello world"))))])

  (match (l/test-compile ::Main (::lang/site :client (undefined?)))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :cljs.core/undefined?))))])

  (match (l/test-compile ::Main (lang/->cljs-env) (::lang/site :client (undefined?)))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :cljs.core/undefined? (r/pure cljs.core/undefined?)))))])

  (match (l/test-compile ::Main (e/server (let [x 1] (e/client x))))
    `[(r/cdef 0 [] [] :server
        (fn [~'frame] (r/pure 1)))])

  (match (l/test-compile ::Main (name (e/server :foo)))
    `[(r/cdef 0 [:server] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure :foo))
          (r/ap (r/lookup ~'frame :clojure.core/name (r/pure clojure.core/name))
            (r/node ~'frame 0))))])

  (match (l/test-compile ::Main (prn (e/client (::lang/call (e/server (e/ctor nil))))))
    `[(r/cdef 0 [:server :client] [:client] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 0 (r/node ~'frame 0))
          (r/define-node ~'frame 1 (r/join (r/call ~'frame 0)))
          (r/ap (r/lookup ~'frame :clojure.core/prn (r/pure prn)) (r/node ~'frame 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure nil)))])

  (match (l/test-compile ::Main (e/pure (e/server 2)))
    `[(r/cdef 0 [:server] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 2))
          (r/pure (r/incseq ~'frame (r/node ~'frame 0)))))])
  )

(tests "test-let"
  (match (l/test-compile ::Main (::lang/site :client (let [a :foo] [a a])))
    `[(r/cdef 0 [:client] [] :client
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure :foo))
          (r/ap (r/pure clojure.core/vector)
            (r/node ~'frame 0) (r/node ~'frame 0))))])

  (match (l/test-compile ::Main (let [a :foo] [a a]))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure :foo))
          (r/ap (r/pure clojure.core/vector)
            (r/node ~'frame 0) (r/node ~'frame 0))))])

  (match (l/test-compile ::Main (let [a (let [b :foo] [b b])] [a a]))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure :foo))
          (r/define-node ~'frame 1 (r/ap (r/pure clojure.core/vector)
                                     (r/node ~'frame 0) (r/node ~'frame 0)))
          (r/ap (r/pure clojure.core/vector)
            (r/node ~'frame 1) (r/node ~'frame 1))))])

  (match (l/test-compile ::Main (let [a 1] a))
    `[(r/cdef 0 [] [] nil (fn [~'frame] (r/pure 1)))])

  (match (l/test-compile ::Main (e/client (let [a 1] (e/server (prn a)))))
    `[(r/cdef 0 [:client] [] :server
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/ap (r/lookup ~'frame :clojure.core/prn (r/pure clojure.core/prn))
            (r/node ~'frame 0))))])

  (match (l/test-compile ::Main (e/client (let [x "Hello", y "world"] [x y])))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/pure (clojure.core/vector "Hello" "world"))))])

  (match (l/test-compile ::Main (e/client (let [a (e/server :foo)] (e/server (prn a)))))
    `[(r/cdef 0 [:server] [] :server
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure :foo))
          (r/ap (r/lookup ~'frame :clojure.core/prn (r/pure prn)) (r/node ~'frame 0))))])

  (match (l/test-compile ::Main (concat (let [x 1] [x x]) (let [y 2] [y y])))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/define-node ~'frame 1 (r/pure 2))
          (r/ap (r/lookup ~'frame :clojure.core/concat (r/pure clojure.core/concat))
            (r/ap (r/pure clojure.core/vector)
              (r/node ~'frame 0)
              (r/node ~'frame 0))
            (r/ap (r/pure clojure.core/vector)
              (r/node ~'frame 1)
              (r/node ~'frame 1)))))]))

(tests "test-pure"
  (match (l/test-compile ::Main (::lang/site :client (::lang/pure :foo)))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/pure (r/pure :foo))))]))

(tests "test-ctor"
  (match (l/test-compile ::Main (::lang/ctor :foo))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor a)))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor (let [a 2] a))))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure 2)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor (::lang/ctor a))))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 2 (r/free ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor [a (let [a 2] (::lang/ctor a))])))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0)))))
      (r/cdef 1 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 2))
          (r/ap (r/pure clojure.core/vector)
            (r/free ~'frame 0)
            (r/pure (r/ctor ::Main 2 (r/node ~'frame 0))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor (::lang/ctor (::lang/ctor a)))))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 2 (r/free ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 3 (r/free ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1, b 2] (::lang/ctor [a (::lang/ctor b)])))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/define-node ~'frame 1 (r/pure 2))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0) (r/node ~'frame 1)))))
      (r/cdef 2 [] [] nil
        (fn [~'frame]
          (r/ap (r/pure clojure.core/vector)
            (r/free ~'frame 0)
            (r/pure (r/ctor ::Main 2 (r/free ~'frame 1))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1, b 2] (::lang/ctor [b (::lang/ctor a)])))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 2))
          (r/define-node ~'frame 1 (r/pure 1))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0) (r/node ~'frame 1)))))
      (r/cdef 2 [] [] nil
        (fn [~'frame]
          (r/ap (r/pure clojure.core/vector)
            (r/free ~'frame 0)
            (r/pure (r/ctor ::Main 2 (r/free ~'frame 1))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [x (::lang/ctor :foo)] x))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))])

  (match (l/test-compile ::Main (let [fizz "fizz", buzz "buzz"]
                                  (e/ctor (str fizz buzz))))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure "fizz"))
          (r/define-node ~'frame 1 (r/pure "buzz"))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0) (r/node ~'frame 1)))))
      (r/cdef 2 [] [] nil
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/str (r/pure clojure.core/str))
            (r/free ~'frame 0)
            (r/free ~'frame 1))))]))

(tests "test-call"
  (match (l/test-compile ::Main (::lang/call (::lang/ctor :foo)))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))])

  (match (l/test-compile ::Main (let [x (::lang/ctor :foo), y x] (::lang/call y)))
    `[(r/cdef 0 [nil] [nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 0 (r/node ~'frame 0))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :foo)))])

  (match (l/test-compile ::Main (vector 1 (::lang/call (::lang/ctor :foo))))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure clojure.core/vector))
            (r/pure 1)
            (r/join (r/call ~'frame 0)))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :foo)))])

  (match (l/test-compile ::Main (let [x (::lang/ctor :foo)] [(::lang/call x) (::lang/call x)]))
    `[(r/cdef 0 [nil] [nil nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 0 (r/node ~'frame 0))
          (r/define-call ~'frame 1 (r/node ~'frame 0))
          (r/ap (r/pure clojure.core/vector)
            (r/join (r/call ~'frame 0))
            (r/join (r/call ~'frame 1)))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :foo)))])

  (match (l/test-compile ::Main [(::lang/call (::lang/ctor :foo)) (::lang/call (::lang/ctor :bar))])
    `[(r/cdef 0 [] [nil nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 1 (r/pure (r/ctor ::Main 2)))
          (r/ap (r/pure clojure.core/vector)
            (r/join (r/call ~'frame 0))
            (r/join (r/call ~'frame 1)))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :bar)))])

  (match (l/test-compile ::Main (let [a :foo] (::lang/call (::lang/ctor (::lang/ctor a)))))
    `[(r/cdef 0 [nil] [nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure :foo))
          (r/define-call ~'frame 0 (r/pure (r/ctor ::Main 1 (r/node ~'frame 0))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 2 (r/free ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))]))

(tests "test-conditionals"
  ;; ({nil (ctor :y)} :x (ctor :z))
  (match (l/test-compile ::Main (case :x nil :y :z))
    `[(r/cdef 0 [nil] [nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 0 (r/ap (r/ap (r/pure hash-map) (r/pure 'nil) (r/node ~'frame 0))
                                   (r/pure :x)
                                   (r/pure (r/ctor ::Main 2))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil (fn [~'frame] (r/pure :y)))
      (r/cdef 0 [] [] nil (fn [~'frame] (r/pure :z)))])

  (match (l/test-compile ::Main (case 'foo (foo bar) :share-this :else))
    `[(r/cdef 0 [nil] [nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 0 (r/ap (r/ap (r/pure clojure.core/hash-map)
                                           (r/pure '~'foo) (r/node ~'frame 0)
                                           (r/pure '~'bar) (r/node ~'frame 0))
                                     (r/pure '~'foo)
                                     (r/pure (r/ctor ::Main 2))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :share-this)))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :else)))])

  (match (l/test-compile ::Main (if 1 2 3))
    `[(r/cdef 0 [nil] [nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 0 (r/ap (r/ap (r/pure clojure.core/hash-map)
                                           (r/pure 'nil) (r/node ~'frame 0)
                                           (r/pure 'false) (r/node ~'frame 0))
                                     (r/pure 1)
                                     (r/pure (r/ctor ::Main 2))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure 3)))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure 2)))]))

(tests "test-lookup"
  (match (l/test-compile ::Main (::lang/lookup 0))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/lookup ~'frame 0)))]))

(defn should-work-in-cljs [])

(tests "test-unsited-cljs-fn"
  (match (l/test-compile ::Main (should-work-in-cljs))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/ap (r/lookup ~'frame ::should-work-in-cljs
                  (r/pure should-work-in-cljs)))))]))

(tests "test-ctor-site-clearing"
  (match (l/test-compile ::Main (e/client (e/ctor (let [x 1] [x x]))))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/ap (r/pure clojure.core/vector)
            (r/node ~'frame 0)
            (r/node ~'frame 0))))]))

(tests "test-binding"
  (match (l/test-compile ::Main
           (binding [inc dec, dec inc]
             (inc (dec 0))))
    `[(r/cdef 0 [nil nil] [nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/lookup ~'frame :clojure.core/dec (r/pure dec)))
          (r/define-node ~'frame 1 (r/lookup ~'frame :clojure.core/inc (r/pure inc)))
          (r/define-call ~'frame 0 (r/ap (r/pure (fn* []
                                                   (r/bind (r/ctor ::Main 1)
                                                     :clojure.core/inc (r/node ~'frame 0)
                                                     :clojure.core/dec (r/node ~'frame 1))))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/inc (r/pure inc))
            (r/ap (r/lookup ~'frame :clojure.core/dec (r/pure dec))
              (r/pure 0)))))]))

(tests "test-ap-collapse"
  (match (l/test-compile ::Main [1 2])
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (clojure.core/vector 1 2))))])
  (match (l/test-compile ::Main ((::lang/static-vars prn) 1))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/ap (r/pure (fn* [] (~'prn 1))))))]))

(tests "ordering"
  (match (l/test-compile ::Main (::lang/call (::lang/call (::lang/ctor (::lang/ctor :foo)))))
    `[(r/cdef 0 [] [nil nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/ctor ::Main 1))) ; must come first
          (r/define-call ~'frame 1 (r/join (r/call ~'frame 0)))
          (r/join (r/call ~'frame 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 2))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))])
  (match (l/test-compile ::Main (let [x 1, y 2] [y x x y]))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 2))
          (r/define-node ~'frame 1 (r/pure 1))
          (r/ap (r/pure clojure.core/vector)
            (r/node ~'frame 0) (r/node ~'frame 1) (r/node ~'frame 1) (r/node ~'frame 0))))])
  (match (l/test-compile ::Main (let [x 1] [(::lang/call (::lang/ctor 1)) x x (::lang/call (::lang/ctor 2))]))
    `[(r/cdef 0 [nil] [nil nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/ctor :hyperfiddle.electric.impl.compiler-test/Main 1)))
          (r/define-node ~'frame 0 (r/pure 1))
          (r/define-call ~'frame 1 (r/pure (r/ctor :hyperfiddle.electric.impl.compiler-test/Main 2)))
          (r/ap (r/pure clojure.core/vector)
            (r/join (r/call ~'frame 0))
            (r/node ~'frame 0)
            (r/node ~'frame 0)
            (r/join (r/call ~'frame 1)))))
      (r/cdef 0 [] [] nil (fn [~'frame] (r/pure 1)))
      (r/cdef 0 [] [] nil (fn [~'frame] (r/pure 2)))]))

(tests "cc-fn-wrapping-js-constructor"
  (match (l/test-compile ::Main (e/client (fn [] (js/Date.))))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame] (r/pure (vector))))]) ; shim, no conveyed values
  (match (l/test-compile ::Main (let [x 1] (e/client (fn [] (js/Date. x)))))
    `[(r/cdef 0 [nil] [] :client
    (fn [~'frame]
     (r/define-node ~'frame 0 (r/pure 1))
     (r/ap (r/pure vector) (r/node ~'frame 0))))])) ; shim, conveyed `x`

(comment

  (let [ts (l/code->ts {} (prn :hello))
        ap-uid (lang/e->uid ts (ts/find1 ts ::lang/type ::lang/ap))]
    (match (ts/->node ts (ts/find1 ts ::lang/source-map-of ap-uid))
      {::lang/line `tm/_, ::lang/column `tm/_}))


  (number? (-> (l/compile-client-source-map (prn "hello world")) first ::lang/line)) := true
  (let [sm (l/compile-client-source-map (let [x "Hello world", y "Hello world"] [x y]))
        line (-> sm first ::lang/line)]
    (number? line) := true                      ; x
    (-> sm second ::lang/line) := line          ; y
    (-> sm (nth 2) first ::lang/line) := line   ; ap
    (-> sm (nth 2) second ::lang/line) := line) ; []

  (let [sm (l/compile-client-source-map (::lang/ctor :foo))
        line (-> sm first ::lang/line)]
    (number? line) := true              ; static :foo
    (-> sm second ::lang/line) := line) ; ctor

  (let [sm (l/compile-client-source-map (::lang/call (::lang/ctor :foo)))
        line (-> sm first ::lang/line)]
    (number? line) := true               ; static :foo
    (-> sm (nth 1) ::lang/line) := line  ; ctor
    (-> sm (nth 2) ::lang/line) := line) ; call

  (let [sm (l/compile-client-source-map (::lang/pure :foo))]
    (number? (-> sm ffirst ::lang/line)) := true) ; pure

  (let [sm (l/compile-client-source-map (::lang/join (::lang/pure :foo)))]
    (number? (-> sm ffirst ::lang/line)) := true) ; join

  (let [sm (l/compile-client-source-map (case :x nil :y :z))
        line (-> sm first ::lang/line)]
    (every? #(= line (::lang/line (cond-> % (seq? %) first))) sm) := true) ; every toplevel case flow

  (let [sm (l/compile-client-source-map (-> 1 inc))]
    (number? (-> sm ffirst ::lang/line)) := true) ; ap

  (let [sm (l/compile-client-source-map (do 1 2))]
    (every? number? (eduction (map #(cond-> % (seq? %) first)) (map ::lang/line) (first sm)))) ; every toplevel do flow

  ;; TODO `loop` needs binding and electric defs
  ;; (let [sm (l/compile-client-with-source-map (loop [x 1] (recur (inc x))))])

  ;; TODO `set!` needs cc/fn
  ;; (let [sm (l/compile-client-with-source-map (set! (.-x (Object.)) 1))])

  (l/test-compile ::Main (e/letfn [(Foo [] Foo)] Foo))

  (l/test-compile ::Main (e/$ (e/fn Foo ([] (e/$ Foo 10)) ([x] (inc x))) 100))
  (l/test-compile ::Main (e/$ (::lang/mklocal Foo (::lang/bindlocal Foo (e/fn [x] Foo) Foo))))
  )

(prn :ok)
