(ns hyperfiddle.electric.impl.compiler-test
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.incseq :as i]
            #?(:clj [contrib.triple-store :as ts])
            [hyperfiddle.electric.impl.lang3 :as lang]
            [hyperfiddle.electric.impl.runtime3 :as r]
            [hyperfiddle.electric-local-def3 :as l]
            #?(:clj [hyperfiddle.electric.impl.compiler-test-clj :refer [cannot-be-unsited]]
               :cljs [hyperfiddle.electric.impl.compiler-test-cljs :refer [cannot-be-unsited]])
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [hyperfiddle.electric.impl.expand-require-referred :as ref :refer [referred referred-fn]]
            #?(:clj [contrib.test-match :as tm])
            #?(:cljs [goog.math :as gm])
            [fipp.edn]
            [missionary.core :as m]
            [hyperfiddle.electric.impl.cljs-analyzer2 :as cljs-ana]
            [clojure.string :as str])
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
          (r/ap '{} (r/pure (fn* [] (~'prn "Hello world"))))))])
  (match (l/test-compile ::Main (prn (e/client 1)))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/ap '{} (r/pure (fn* [] (~'prn 1))))))]))

(tests
  "test-join"
  (match (l/test-compile ::Main {'!x (atom 0)} (::lang/join (i/fixed (m/watch !x))))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/join (r/pure (~'i/fixed (~'m/watch ~'!x))))))]))

(tests "test-siting"
  (match (l/test-compile ::Main (::lang/site :client "Hello world"))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/pure "Hello world")))])

  ;; {:locals {}, :ns {:name (ns-name *ns*)}, ::lang/print-db true, ::lang/print-analysis true}
  (match (l/test-compile ::Main (e/client (prn "Hello world")))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/ap '{} (r/pure (fn* [] (r/cannot-resolve "Hello world"))))))])

  (match (l/test-compile ::Main (e/server (prn "Hello world")))
    `[(r/cdef 0 [] [] :server
        (fn [~'frame]
          (r/ap '{} (r/pure (fn* [] (~'prn "Hello world"))))))])

  (match (l/test-compile ::Main (lang/->cljs-env) (::lang/site :client (undefined?)))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/ap '{} (r/pure ~'undefined?))))])

  (match (l/test-compile ::Main (e/server (let [x 1] (e/client x))))
    `[(r/cdef 0 [] [] :server
        (fn [~'frame] (r/pure 1)))])

  (match (l/test-compile ::Main (name (e/server :foo)))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (~'name :foo))))])

  (match (l/test-compile ::Main (prn (e/client (::lang/call (e/server (e/ctor nil))))))
    `[(r/cdef 0 [:server :client] [:client] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 0 (r/node ~'frame 0))
          (r/define-node ~'frame 1 '{} (r/join (r/call ~'frame 0)))
          (r/ap '{} (r/pure ~'prn) (r/node ~'frame 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure nil)))])

  (match (l/test-compile ::Main (e/pure (e/server 2)))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/incseq ~'frame (r/pure 2)))))])

  (match (l/test-compile ::Main (let [x (e/server (identity 1))] (inc x)))
    `[(r/cdef 0 [:server] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'identity 1)))
          (r/ap '{} (r/pure ~'inc)
            (r/node ~'frame 0))))]))

(tests "test-let"
  (match (l/test-compile ::Main (let [a :foo] [a a]))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (vector :foo :foo))))])

  (match (l/test-compile ::Main (let [a (let [b :foo] [b b])] [a a]))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (vector :foo :foo)))
          (r/ap '{} (r/pure clojure.core/vector)
            (r/node ~'frame 0) (r/node ~'frame 0))))])

  (match (l/test-compile ::Main (let [a 1] a))
    `[(r/cdef 0 [] [] nil (fn [~'frame] (r/pure 1)))])

  (match (l/test-compile ::Main (e/client (let [a 1] (e/server (prn a)))))
    `[(r/cdef 0 [] [] :server
        (fn [~'frame]
          (r/ap '{} (r/pure (fn* [] (~'prn 1))))))])

  (match (l/test-compile ::Main (let [x "Hello", y "world"] [x y]))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (clojure.core/vector "Hello" "world"))))])

  (match (l/test-compile ::Main (e/client (let [a (e/server :foo)] (e/server (prn a)))))
    `[(r/cdef 0 [] [] :server
        (fn [~'frame]
          (r/ap '{} (r/pure (fn* [] (~'prn :foo))))))])

  (match (l/test-compile ::Main (concat (let [x 1] [x x]) (let [y 2] [y y])))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (~'concat (vector 1 1) (vector 2 2)))))]))

(tests "test-pure"
  (match (l/test-compile ::Main (::lang/pure :foo))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/pure :foo))))]))

(defn ->fresh-sym [] (let [->id (lang/->->id)] (fn self ([] (self "g")) ([s] (symbol (str s (->id)))))))

;; NEXT
(tests "test-ctor"
  (match (l/test-compile ::Main (::lang/ctor :foo))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor a)))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure 1)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor (let [a 2] a))))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure 2)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor (::lang/ctor a))))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 2))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure 1)))])

  (match (l/test-compile ::Main (let [a (inc 1)] (::lang/ctor (::lang/ctor a))))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'inc 1)))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 2 (r/free ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main {:locals {}, :ns {:name (ns-name *ns*)}, ::lang/->sym (->fresh-sym)}
           (let [a (inc 1)] (::lang/ctor [a (let [a (inc 2)] (::lang/ctor a))])))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'inc 1)))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0)))))
      (r/cdef 1 [nil] [] nil
        (fn [~'frame]
          (let [~'init-fn0 (fn ~'init-fn0 [~'arg1] (vector ~'arg1 (r/ctor ::Main 2 (r/node ~'frame 0))))]
            (r/define-node ~'frame 0 '{} (r/pure (~'inc 2)))
            (r/ap '{} (r/pure ~'init-fn0) (r/free ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a (inc 1)] (::lang/ctor (::lang/ctor (::lang/ctor a)))))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'inc 1)))
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

  (match (l/test-compile ::Main {:locals {}, :ns {:name (ns-name *ns*)}, ::lang/->sym (->fresh-sym)}
           (let [a (inc 1), b (inc 2)] (::lang/ctor [a (::lang/ctor b)])))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'inc 1)))
          (r/define-node ~'frame 1 '{} (r/pure (~'inc 2)))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0) (r/node ~'frame 1)))))
      (r/cdef 2 [] [] nil
        (fn [~'frame]
          (let [~'init-fn0 (fn ~'init-fn0 [~'arg1] (vector ~'arg1 (r/ctor ::Main 2 (r/free ~'frame 1))))]
            (r/ap '{} (r/pure ~'init-fn0)
              (r/free ~'frame 0)))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main {:locals {}, :ns {:name (ns-name *ns*)}, ::lang/->sym (->fresh-sym)}
           (let [a (inc 1), b (inc 2)] (::lang/ctor [b (::lang/ctor a)])))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'inc 2)))
          (r/define-node ~'frame 1 '{} (r/pure (~'inc 1)))
          (r/pure (r/ctor ::Main 1 (r/node ~'frame 0) (r/node ~'frame 1)))))
      (r/cdef 2 [] [] nil
        (fn [~'frame]
          (let [~'init-fn0 (fn ~'init-fn0 [~'arg1] (vector ~'arg1 (r/ctor ::Main 2 (r/free ~'frame 1))))]
            (r/ap '{} (r/pure ~'init-fn0)
              (r/free ~'frame 0)))))
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
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (~'str "fizz" "buzz"))))]))

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
          (r/define-node ~'frame 0 '{} (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 0 (r/node ~'frame 0))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :foo)))])

  (match (l/test-compile ::Main {:locals {}, :ns {:name (ns-name *ns*)}, ::lang/->sym (->fresh-sym)}
           (vector 1 (::lang/call (::lang/ctor :foo))))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (let [~'init-fn0 (fn ~'init-fn0 [~'arg1] (~'vector 1 ~'arg1))]
            (r/define-call ~'frame 0 (r/pure (r/ctor ::Main 1)))
            (r/ap '{} (r/pure ~'init-fn0)
              (r/join (r/call ~'frame 0))))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :foo)))])

  (match (l/test-compile ::Main (let [x (::lang/ctor :foo)] [(::lang/call x) (::lang/call x)]))
    `[(r/cdef 0 [nil] [nil nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 0 (r/node ~'frame 0))
          (r/define-call ~'frame 1 (r/node ~'frame 0))
          (r/ap '{} (r/pure clojure.core/vector)
            (r/join (r/call ~'frame 0))
            (r/join (r/call ~'frame 1)))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :foo)))])

  (match (l/test-compile ::Main [(::lang/call (::lang/ctor :foo)) (::lang/call (::lang/ctor :bar))])
    `[(r/cdef 0 [] [nil nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 1 (r/pure (r/ctor ::Main 2)))
          (r/ap '{} (r/pure clojure.core/vector)
            (r/join (r/call ~'frame 0))
            (r/join (r/call ~'frame 1)))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :bar)))])

  (match (l/test-compile ::Main (let [a :foo] (::lang/call (::lang/ctor (::lang/ctor a)))))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/ctor ::Main 2))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))]))

(tests "test-conditionals"
  ;; ({nil (ctor :y)} :x (ctor :z))
  (match (l/test-compile ::Main (case :x nil :y :z))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0
            (r/ap '{}
              (r/pure
                (fn* [] (tm/_ :x (r/ctor ::Main 1) (r/ctor ::Main 2))))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :y)))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :z)))])

  (match (l/test-compile ::Main (case 'foo (foo bar) :share-this :else))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0
            (r/ap '{}
              (r/pure (fn* [] (tm/_ '~'foo (r/ctor ::Main 1) (r/ctor ::Main 2))))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :share-this)))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :else)))])

  (match (l/test-compile ::Main (if 1 2 3))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0
            (r/ap '{}
              (r/pure
                (fn* [] (tm/_ 1 (r/ctor ::Main 1) (r/ctor ::Main 2))))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure 3)))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure 2)))]))

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
          (r/ap '{} (r/pure ~'should-work-in-cljs))))]))

(tests "test-ctor-site-clearing"
  (match (l/test-compile ::Main (e/client (e/ctor (let [x (inc 1)] [x x]))))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/pure (r/ctor ::Main 1))))
      (r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'inc 1)))
          (r/ap '{} (r/pure clojure.core/vector)
            (r/node ~'frame 0)
            (r/node ~'frame 0))))]))

(e/declare einc edec)
(tests "test-binding"
  (match (l/test-compile ::Main
           (binding [einc dec, edec inc]
             (einc (edec 0))))
    `[(r/cdef 0 [nil nil] [nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure ~'dec))
          (r/define-node ~'frame 1 '{} (r/pure ~'inc))
          (r/define-call ~'frame 0
            (r/pure (r/bind (r/ctor ::Main 1)
                      ::einc (r/node ~'frame 0)
                      ::edec (r/node ~'frame 1))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/ap '{} (r/lookup ~'frame ::einc (r/pure (r/resolve ~'frame ::einc)))
            (r/ap '{} (r/lookup ~'frame ::edec (r/pure (r/resolve ~'frame ::edec)))
              (r/pure 0)))))]))

(tests "test-ap-collapse"
  (match (l/test-compile ::Main [1 2])
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (clojure.core/vector 1 2))))])
  (match (l/test-compile ::Main ((::lang/static-vars prn) 1))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/ap '{} (r/pure (fn* [] (~'prn 1))))))]))

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
  (match (l/test-compile ::Main (let [x (inc 1), y (dec 2)] [y x x y]))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'dec 2)))
          (r/define-node ~'frame 1 '{} (r/pure (~'inc 1)))
          (r/ap '{} (r/pure clojure.core/vector)
            (r/node ~'frame 0) (r/node ~'frame 1) (r/node ~'frame 1) (r/node ~'frame 0))))])
  (match (l/test-compile ::Main (let [x (inc 1)] [(::lang/call (::lang/ctor 1)) x x (::lang/call (::lang/ctor 2))]))
    `[(r/cdef 0 [nil] [nil nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/ctor ::Main 1)))
          (r/define-node ~'frame 0 '{} (r/pure (~'inc 1)))
          (r/define-call ~'frame 1 (r/pure (r/ctor ::Main 2)))
          (r/ap '{} (r/pure clojure.core/vector)
            (r/join (r/call ~'frame 0))
            (r/node ~'frame 0)
            (r/node ~'frame 0)
            (r/join (r/call ~'frame 1)))))
      (r/cdef 0 [] [] nil (fn [~'frame] (r/pure 1)))
      (r/cdef 0 [] [] nil (fn [~'frame] (r/pure 2)))]))

(tests "cc-fn-wrapping-js-constructor"
  (match (l/test-compile ::Main (e/client (fn [] (js/Date.))))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame] (r/ap '{} (r/pure r/cannot-resolve))))]) ; shim, no conveyed values
  (match (l/test-compile ::Main (let [x (inc 1)] (e/client (fn [] (js/Date. x)))))
    `[(r/cdef 0 [nil] [] :client
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'inc 1)))
          (r/ap '{} (r/pure r/cannot-resolve) (r/node ~'frame 0))))])) ; shim, conveyed `x`

(tests "peers-dont-compile-foreign-code"
  (match (l/test-compile ::Main (merge e/web-config (lang/normalize-env {}) {:js-globals {}})
           (e/server (java.time.Instant/ofEpochMilli 11)))
    `[(r/cdef 0 [] [] :server
        (fn [~'frame]
          (r/ap '{} (r/pure (fn* [] (r/cannot-resolve 11))))))])

  (match (l/test-compile ::Main (merge e/web-config (lang/normalize-env {}) {:js-globals {}})
           (e/server #(inc e/Count)))
    `[(r/cdef 0 [] [] :server
        (fn [~'frame]
          (r/ap '{} (r/pure r/cannot-resolve)
            (r/lookup ~'frame :hyperfiddle.electric3/Count
              (r/pure (r/resolve ~'frame :hyperfiddle.electric3/Count))))))])

  (match (l/test-compile ::Main (lang/->cljs-env) (java.nio.file.Path/of "./src"))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (vector "./src"))))])

  (match (l/test-compile ::Main (merge e/web-config (lang/normalize-env {}) {:js-globals {}})
           String)
    `[(r/cdef 0 [] [] :server
        (fn [~'frame]
          (r/pure r/cannot-resolve)))])
  )

(tests "::lang/tag"
  (match (l/test-compile ::Main [(::lang/tag)
                                 (::lang/tag)
                                 (::lang/call (::lang/ctor 1))
                                 (::lang/tag)
                                 (::lang/call (::lang/ctor 2))])
    `[(r/cdef 0 [] [nil nil nil nil nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 2 (r/pure (r/ctor ::Main 1)))
          (r/define-call ~'frame 4 (r/pure (r/ctor ::Main 2)))
          (r/ap '{} (r/pure vector)
            (r/pure (r/tag ~'frame 0))
            (r/pure (r/tag ~'frame 1))
            (r/join (r/call ~'frame 2))
            (r/pure (r/tag ~'frame 3))
            (r/join (r/call ~'frame 4)))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure 1)))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure 2)))]))

(tests "call order"
  (match (l/test-compile ::Main (let [x (binding [::foo (inc 1)] (::lang/lookup ::foo))]
                                  (prn x)
                                  (prn x)))
    `[(r/cdef 0 [nil nil nil nil nil] [nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure (~'inc 1)))
          (r/define-call ~'frame 0
            (r/pure (r/bind (r/ctor ::Main 1) ::foo (r/node ~'frame 0))))
          (r/define-node ~'frame 1 '{}
            (r/join (r/call ~'frame 0)))
          (r/define-node ~'frame 2 '{}
            (r/ap '{}
              (r/pure ~'prn)
              (r/node ~'frame 1)))
          (r/define-node ~'frame 3 'nil
            (r/join
              (r/pure
                (r/drain
                  (r/incseq ~'frame (r/node ~'frame 2))))))
          (r/define-node ~'frame 4 '{}
            (r/ap '{}
              (r/pure ~'prn)
              (r/node ~'frame 1)))
          (r/join
            (r/join
              (r/pure
                (i/fixed
                  (r/invariant (r/incseq ~'frame (r/node ~'frame 3)))
                  (r/invariant (r/incseq ~'frame (r/node ~'frame 4)))))))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/lookup ~'frame ::foo)))]))

(declare #?(:clj clj-only :cljs cljs-only))
(tests
  "auto-siting of single-peer globals"
  (match (l/test-compile ::Main (e/client (inc clj-only)))
    `[(r/cdef 0 [:server] [] :client
        (fn [~'frame]
          (r/define-node ~'frame 0 '{}
            (r/pure ~'clj-only))
          (r/ap '{} (r/pure r/cannot-resolve)
            (r/node ~'frame 0))))])
  (match (l/test-compile ::Main (merge e/web-config (lang/normalize-env {}) {:js-globals {}})
           (e/client (inc clj-only)))
    `[(r/cdef 0 [:server] [] :client
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure r/cannot-resolve))
          (r/ap '{} (r/pure ~'inc)
            (r/node ~'frame 0))))])
  (match (l/test-compile ::Main (merge e/web-config (lang/normalize-env {}) {:js-globals {}})
           (::lang/site nil (inc clj-only)))
    `[(r/cdef 0 [:server] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure r/cannot-resolve))
          (r/ap '{} (r/pure ~'inc)
            (r/node ~'frame 0))))])
  (match (l/test-compile ::Main (e/server (inc cljs-only)))
    `[(r/cdef 0 [:client] [] :server
        (fn [~'frame]
          (r/define-node ~'frame 0 '{} (r/pure r/cannot-resolve))
          (r/ap '{} (r/pure ~'inc)
            (r/node ~'frame 0))))]))

(def ^:dynamic *d* nil)
(def fenv {:ns {:name 'hyperfiddle.electric.impl.compiler-test}
           ::lang/peers {:client :cljs, :server :clj}
           :locals {'e1 {::lang/electric-let 1}
                    'e2 {::lang/electric-let 2}
                    'x  {}
                    'z  {}}})

(def fenv-jvm (assoc fenv ::lang/current :server))

(defn foreign [o]
  (-> (lang/analyze-foreign (lang/-expand-all-foreign o fenv) fenv)
    (lang/emit-foreign)))

(defn foreign-jvm [o]
  (-> (lang/analyze-foreign (lang/-expand-all-foreign o fenv-jvm) fenv-jvm)
    (lang/emit-foreign)))

(defn foreign-electrified
  ([o] (-> (lang/analyze-foreign (lang/-expand-all-foreign o fenv) fenv)
         (lang/wrap-foreign-for-electric)))
  ([gen o]
   (-> (lang/analyze-foreign (lang/-expand-all-foreign o fenv) fenv)
     (lang/wrap-foreign-for-electric gen))))

(defn foreign-electrified-jvm
  ([o] (-> (lang/analyze-foreign (lang/-expand-all-foreign o fenv-jvm) fenv-jvm)
         (lang/wrap-foreign-for-electric)))
  ([gen o]
   (-> (lang/analyze-foreign (lang/-expand-all-foreign o fenv-jvm) fenv-jvm)
     (lang/wrap-foreign-for-electric gen))))

(def fenv-js (merge (cljs-ana/->cljs-env) fenv {::lang/current :client}))
(defn foreign-js [o]
  (-> (lang/analyze-foreign (lang/-expand-all-foreign o fenv-js) fenv-js)
    (lang/emit-foreign)))

(defn foreign-electrified-js
  ([o] (-> (lang/analyze-foreign (lang/-expand-all-foreign o fenv-js) fenv-js)
         (lang/wrap-foreign-for-electric)))
  ([gen o]
   (-> (lang/analyze-foreign (lang/-expand-all-foreign o fenv-js) fenv-js)
     (lang/wrap-foreign-for-electric gen))))

(tests
  "foreign"
  (foreign '(let [x 1, y 2] 3)) := '(let* [x 1 y 2] 3)
  (foreign '(quote foo)) := ''foo
  (foreign '(fn ([x] 1) ([x y] 2))) := '(fn* ([x] 1) ([x y] 2))
  (foreign '(letfn [(foo [x] 1) (bar [y] 2)] 3)) := '(letfn* [foo (fn* foo ([x] 1)) bar (fn* bar ([y] 2))] 3)
  (foreign '(Math/abs -1)) := '(Math/abs -1)
  (foreign '(. Math abs -1)) := '(Math/abs -1)
  (foreign '(. Math (abs -1))) := '(Math/abs -1)
  (foreign '(binding [x 1 y 2] 3)) := '(binding [x 1 y 2] 3)
  (foreign '(def foo 1)) := '(def foo 1)
  (foreign '(do (def foo 1) 2)) := '(do (def foo 1) 2)
  (foreign '{:a 1 :b 2}) := '{:a 1 :b 2}
  (foreign '[x :y 1]) := '[x :y 1]
  (foreign '#{x :y 1}) := '#{x :y 1}
  (foreign '(. e1 x)) := '(. e1 x)
  (foreign '(.-x e1)) := '(. e1 -x)
  (foreign '(. e1 (isAfter e2))) := '(. e1 isAfter e2)
  (foreign '(. e1 isAfter e2)) := '(. e1 isAfter e2)
  (foreign '(set! x 1)) := '(set! x 1)
  (foreign '(let [o (Object.)] (set! (.-x o) 1))) := '(let* [o (new Object)] (set! (. o -x) 1))
  (foreign '(println 1 (inc 2))) := '(println 1 (inc 2))
  (foreign '(case (-> 1 inc) (2) (-> 2 dec) 3 (-> 3 dec))) := '(case (inc 1) (2) (dec 2) 3 (dec 3))
  (foreign '(case (-> 1 inc) (-> 1 dec)))
  (foreign '(throw (ex-info "hi" {}))) := '(throw (ex-info "hi" {}))
  ;; (foreign '#js{:x 1})
  "jvm"
  (foreign-jvm '(new Object 1 2 3)) := '(new Object 1 2 3)
  "js"
  (swap! lang/!a cljs-ana/purge-ns (ns-name *ns*))
  (cljs-ana/analyze-nsT lang/!a fenv-js (ns-name *ns*))
  (foreign-js '(let [x 1, y 2] 3)) := '(let* [x 1 y 2] 3)
  (foreign-js '(quote foo)) := ''foo
  (foreign-js '(fn ([x] 1) ([x y] 2))) := '(fn* ([x] 1) ([x y] 2))
  (foreign-js '(letfn [(foo [x] 1) (bar [y] 2)] 3)) := '(letfn* [foo (fn* foo ([x] 1)) bar (fn* bar ([y] 2))] 3)
  (foreign-js '(Math/abs -1)) := '(Math/abs -1)
  (foreign-js '(binding [x 1 y 2] 3)) := '(binding [x 1 y 2] 3)
  (foreign-js '(def foo 1)) := '(def foo 1)
  (foreign-js '(do (def foo 1) 2)) := '(do (def foo 1) 2)
  (foreign-js '{:a 1 :b 2}) := '{:a 1 :b 2}
  (foreign-js '(. e1 x)) := '(. e1 x)
  (foreign-js '(.-x e1)) := '(. e1 -x)
  (foreign-js '(. e1 (isAfter e2))) := '(. e1 isAfter e2)
  (foreign-js '(. e1 isAfter e2)) := '(. e1 isAfter e2)
  (foreign-js '(let [o (js/Object.)] (set! (.-x o) 1))) := '(let* [o (new js/Object)] (set! (. o -x) 1))
  (foreign-js '(new js/Object 1 2 3)) := '(new js/Object 1 2 3)
  (foreign-js '(println 1 (inc 2))) := '(println 1 (inc 2))
  (foreign-js '(case (-> 1 inc) (2) (-> 2 dec) 3 (-> 3 dec))) := '(case (inc 1) (2) (dec 2) 3 (dec 3))
  (foreign-js '(case (-> 1 inc) (-> 1 dec)))
  (foreign-js '(throw (ex-info "hi" {}))) := '(throw (ex-info "hi" {}))
  )

(defn consuming [v*] (let [v* (atom v*)] (fn [_] (ffirst (swap-vals! v* next)))))

(e/defn p [])

(tests
  "foreign electrified"
  (foreign-electrified (consuming ['plus]) '(p 2 3))
  := `[(~'plus 2 3) [~'plus] [p]]

  (foreign-electrified (consuming ['plus]) '(p hyperfiddle.electric.impl.compiler-test/p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified (consuming ['plus]) '(hyperfiddle.electric.impl.compiler-test/p p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified (consuming ['plus]) '(p p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified (consuming ['plus]) '(hyperfiddle.electric.impl.compiler-test/p hyperfiddle.electric.impl.compiler-test/p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified nil 'e1)
  := '[e1 [e1] [e1]]

  (foreign-electrified nil '[e1 e1])
  := '[[e1 e1] [e1] [e1]]

  (foreign-electrified (consuming ['plus]) '(hyperfiddle.electric.impl.compiler-test/p e1 3))
  := `[(~'plus ~'e1 3) [~'plus ~'e1] [p ~'e1]]

  (foreign-electrified (consuming ['plus]) '(p e1 3))
  := `[(~'plus ~'e1 3) [~'plus ~'e1] [p ~'e1]]

  "jvm"
  (foreign-electrified-jvm (consuming ['plus]) '(p 2 3))
  := `[(~'plus 2 3) [~'plus] [p]]

  (foreign-electrified-jvm (consuming ['plus]) '(p hyperfiddle.electric.impl.compiler-test/p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified-jvm (consuming ['plus]) '(hyperfiddle.electric.impl.compiler-test/p p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified-jvm (consuming ['plus]) '(p p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified-jvm (consuming ['plus]) '(hyperfiddle.electric.impl.compiler-test/p hyperfiddle.electric.impl.compiler-test/p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified-jvm nil 'e1)
  := '[e1 [e1] [e1]]

  (foreign-electrified-jvm nil '[e1 e1])
  := '[[e1 e1] [e1] [e1]]

  (foreign-electrified-jvm (consuming ['plus]) '(hyperfiddle.electric.impl.compiler-test/p e1 3))
  := `[(~'plus ~'e1 3) [~'plus ~'e1] [p ~'e1]]

  (foreign-electrified-jvm (consuming ['plus]) '(p e1 3))
  := `[(~'plus ~'e1 3) [~'plus ~'e1] [p ~'e1]]

  "js"
  (foreign-electrified-js (consuming ['plus]) '(p 2 3))
  := `[(~'plus 2 3) [~'plus] [p]]

  (foreign-electrified-js (consuming ['plus]) '(p hyperfiddle.electric.impl.compiler-test/p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified-js (consuming ['plus]) '(hyperfiddle.electric.impl.compiler-test/p p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified-js (consuming ['plus]) '(p p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified-js (consuming ['plus]) '(hyperfiddle.electric.impl.compiler-test/p hyperfiddle.electric.impl.compiler-test/p))
  := `[(~'plus ~'plus) [~'plus] [p]]

  (foreign-electrified-js nil 'e1)
  := '[e1 [e1] [e1]]

  (foreign-electrified-js nil '[e1 e1])
  := '[[e1 e1] [e1] [e1]]

  (foreign-electrified-js (consuming ['plus]) '(hyperfiddle.electric.impl.compiler-test/p e1 3))
  := `[(~'plus ~'e1 3) [~'plus ~'e1] [p ~'e1]]

  (foreign-electrified-js (consuming ['plus]) '(p e1 3))
  := `[(~'plus ~'e1 3) [~'plus ~'e1] [p ~'e1]]

  (foreign-electrified-js (consuming '[a]) '(set! consuming e1))
  := '[(set! consuming e1) [e1] [e1]]

  )

(def unsited-fenv fenv)
(def unsited-fenv-js (assoc fenv-js ::lang/current nil))

(defn foreign-electrified-unsited
  ([o] (-> (lang/analyze-foreign (lang/-expand-all-foreign o unsited-fenv) unsited-fenv)
         (lang/wrap-foreign-for-electric)))
  ([gen o] (-> (lang/analyze-foreign (lang/-expand-all-foreign o unsited-fenv) unsited-fenv)
             (lang/wrap-foreign-for-electric gen))))

(defn foreign-electrified-unsited-js
  ([o] (-> (lang/analyze-foreign (lang/-expand-all-foreign o unsited-fenv-js) unsited-fenv-js)
         (lang/wrap-foreign-for-electric)))
  ([gen o] (-> (lang/analyze-foreign (lang/-expand-all-foreign o unsited-fenv-js) unsited-fenv-js)
             (lang/wrap-foreign-for-electric gen))))

(tests
  "unsited edge cases"

  (foreign-electrified-unsited (consuming '[]) '(fn [x] (goog.object/create e1)))
  := '[(fn* ([x] ((hyperfiddle.electric.impl.runtime3/cannot-resolve-fn (quote goog.object/create)) e1))) [e1] [e1]]

  (foreign-electrified-unsited (consuming '[]) '(fn [x] (try (/ 1 0) (catch js/Error e e))))
  := '[(fn* ([x] (try (/ 1 0) (catch Throwable e e)))) [] []]

  (foreign-electrified-unsited (consuming '[]) '(set! (.-x (-> [(js/Object.)] first)) 2))
  := '[(set! (. (first [(new Object)]) -x) 2) [] []]

  (foreign-electrified-unsited (consuming '[]) '(set! (.-title js/document) e1))
  := '[(set! (. (hyperfiddle.electric.impl.runtime3/cannot-resolve-fn (quote js/document)) -title) e1) [e1] [e1]]

  (foreign-electrified-unsited-js (consuming '[]) '(fn [x] (Thread/sleep e1)))
  := '[(fn* ([x] (. (hyperfiddle.electric.impl.runtime3/cannot-resolve-fn (quote Thread)) sleep e1))) [e1] [e1]]

  (foreign-electrified-unsited-js (consuming '[]) '(fn [x] (try (/ 1 0) (catch Exception e e))))
  := '[(fn* ([x] (try (/ 1 0) (catch :default e e)))) [] []]

  (foreign-electrified-unsited-js (consuming '[]) '(fn [x] (try (/ 1 0) (catch Exception e e) (catch Throwable e e))))
  := '[(fn* ([x] (try (/ 1 0) (catch :default e e)))) [] []]

  (foreign-electrified-unsited-js (consuming '[]) '(set! (.-x (-> [(java.awt.Point. 0 2)] first)) 2))
  := '[(set! (. (first [(new js/Object 0 2)]) -x) 2) [] []]
  )

(tests
  "binding requires an electric var"
  (try (l/test-compile ::Main (binding [inc dec] (inc 1)))
       (catch ExceptionInfo e (ex-message e)))
  := "[inc] is not an electric var")

#?(:clj
   (tests
     "(.foo x) regression, node count mismatch"
     (let [code (l/test-compile ::Main (lang/->cljs-env)
                  (let [h (inc 1), name_ (e/server (.getName h))]
                    (e/client (prn name_))))]
       (tm/test-match code
         `(fn
            tm/_
            (tm/_ (case tm/_ 0 (r/cdef 0 [nil :server] tm/_&)))))
       := code)))

#?(:clj
   (tests
     "graph misalignment bug"
     (match (l/test-compile ::Main
              (::lang/site nil (js/console.log :extra)))
       `[(r/cdef 0 [] [] nil
           (fn [~'frame]
             (r/ap '{}
               (r/pure (fn* [] (r/cannot-resolve :extra))))))])
     (match (l/test-compile ::Main (lang/->cljs-env)
              (::lang/site nil (js/console.log :extra)))
       `[(r/cdef 0 [] [] nil
           (fn [~'frame]
             (r/ap '{}
               (r/pure (fn* [] ((.bind js/console.log js/console) :extra))))))])))

(prn :ok)
