(ns hyperfiddle.electric.impl.compiler-test
  (:require [hyperfiddle.electic :as-alias e]
            [hyperfiddle.incseq :as i]
            #?(:clj [hyperfiddle.electric.impl.lang-de2 :as lang])
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.electric-local-def-de :as l]
            #?(:clj [hyperfiddle.electric.impl.compiler-test-clj :refer [cannot-be-unsited]]
               :cljs [hyperfiddle.electric.impl.compiler-test-cljs :refer [cannot-be-unsited]])
            [hyperfiddle.rcf :as rcf :refer [tests]]
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

(defmacro match [code matcher]
  `(let [ret# ~code, match# (tm/test-match ret# ~matcher)]
     ;; (t/is (= ret# match#))
     ret# := match#
     (when (not= ret# match#) (fipp.edn/pprint match#))
     match#))

(tests "test-simplest"
  (match (l/test-compile ::Main 1)
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure 1)))])
  (match (l/test-compile ::Main (prn "Hello world"))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/prn (r/pure prn)) (r/pure "Hello world"))))])
  (match (l/test-compile ::Main (prn (::lang/site :client 1)))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/prn (r/pure clojure.core/prn))
            (r/pure 1))))]))

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

  (match (l/test-compile ::Main (::lang/site :server (let [x 1] (::lang/site :client x))))
    `[(r/cdef 0 [] [] :server
        (fn [~'frame] (r/pure 1)))])

  (let [ex (try (l/test-compile ::Main cannot-be-unsited) (catch ExceptionInfo e e))]
    (ex-message ex) := "Unsited symbol `cannot-be-unsited` resolves to different vars on different peers. Please resolve ambiguity by siting the expression using it."))

(tests "test-let"
  (match (l/test-compile ::Main (::lang/site :client (let [a :foo] [a a])))
    `[(r/cdef 0 [:client] [] :client
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure :foo))
          (r/ap (r/lookup ~'frame :clojure.core/vector)
            (r/node ~'frame 0) (r/node ~'frame 0))))])

  (match (l/test-compile ::Main (let [a :foo] [a a]))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure :foo))
          (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure vector))
            (r/node ~'frame 0) (r/node ~'frame 0))))])

  (match (l/test-compile ::Main (let [a (let [b :foo] [b b])] [a a]))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure :foo))
          (r/define-node ~'frame 1 (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure vector))
                                     (r/node ~'frame 0) (r/node ~'frame 0)))
          (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure vector))
            (r/node ~'frame 1) (r/node ~'frame 1))))])

  (match (l/test-compile ::Main (let [a 1] a))
    `[(r/cdef 0 [] [] nil (fn [~'frame] (r/pure 1)))])

  (match (l/test-compile ::Main (::lang/site :client (let [a 1] (::lang/site :server (prn a)))))
    `[(r/cdef 0 [:client] [] :server
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/ap (r/lookup ~'frame :clojure.core/prn (r/pure clojure.core/prn))
            (r/node ~'frame 0))))])

  (match (l/test-compile ::Main (::lang/site :client (let [x "Hello", y "world"] [x y])))
    `[(r/cdef 0 [] [] :client
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/vector)
            (r/pure "Hello") (r/pure "world"))))])

  (match (l/test-compile ::Main (::lang/site :client (let [a (::lang/site :server :foo)] (::lang/site :server (prn a)))))
    `[(r/cdef 0 [] [] :server
        (clojure.core/fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/prn (r/pure clojure.core/prn))
            (r/pure :foo))))])
  (match (l/test-compile ::Main (concat (let [x 1] [x x]) (let [y 2] [y y])))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/define-node ~'frame 1 (r/pure 2))
          (r/ap (r/lookup ~'frame :clojure.core/concat (r/pure clojure.core/concat))
            (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure clojure.core/vector))
              (r/node ~'frame 0)
              (r/node ~'frame 0))
            (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure clojure.core/vector))
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
          (r/pure (r/make-ctor ~'frame ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor a)))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/pure (doto (r/make-ctor ~'frame ::Main 1)
                    (r/define-free 0 (r/node ~'frame 0))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor (let [a 2] a))))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/make-ctor ~'frame ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure 2)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor (::lang/ctor a))))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 1)
                    (r/define-free 0 (r/node ~'frame 0))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 2)
                    (r/define-free 0 (r/free ~'frame 0))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor [a (let [a 2] (::lang/ctor a))])))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 1)
                    (r/define-free 0 (r/node ~'frame 0))))))
      (r/cdef 1 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 2))
          (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure clojure.core/vector))
            (r/free ~'frame 0)
            (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 2)
                      (r/define-free 0 (r/node ~'frame 0)))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1] (::lang/ctor (::lang/ctor (::lang/ctor a)))))
    `[(r/cdef 0 [nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 1)
                    (r/define-free 0 (r/node ~'frame 0))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 2)
                    (r/define-free 0 (r/free ~'frame 0))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 3)
                    (r/define-free 0 (r/free ~'frame 0))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1, b 2] (::lang/ctor [a (::lang/ctor b)])))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 1))
          (r/define-node ~'frame 1 (r/pure 2))
          (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 1)
                    (r/define-free 0 (r/node ~'frame 0))
                    (r/define-free 1 (r/node ~'frame 1))))))
      (r/cdef 2 [] [] nil
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure clojure.core/vector))
            (r/free ~'frame 0)
            (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 2)
                      (r/define-free 0 (r/free ~'frame 1)))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [a 1, b 2] (::lang/ctor [b (::lang/ctor a)])))
    `[(r/cdef 0 [nil nil] [] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure 2))
          (r/define-node ~'frame 1 (r/pure 1))
          (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 1)
                    (r/define-free 0 (r/node ~'frame 0))
                    (r/define-free 1 (r/node ~'frame 1))))))
      (r/cdef 2 [] [] nil
        (fn [~'frame]
          (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure clojure.core/vector))
            (r/free ~'frame 0)
            (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 2)
                      (r/define-free 0 (r/free ~'frame 1)))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))])

  (match (l/test-compile ::Main (let [x (::lang/ctor :foo)] x))
    `[(r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure (r/make-ctor ~'frame ::Main 1))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))]))

(tests "test-call"
  (match (l/test-compile ::Main (::lang/call (::lang/ctor :foo)))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/make-ctor ~'frame ::Main 1)))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))])

  (match (l/test-compile ::Main (let [x (::lang/ctor :foo), y x] (::lang/call y)))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/make-ctor ~'frame ::Main 1)))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :foo)))])

  (match (l/test-compile ::Main (vector 1 (::lang/call (::lang/ctor :foo))))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/make-ctor ~'frame ::Main 1)))
          (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure clojure.core/vector))
            (r/pure 1)
            (r/join (r/call ~'frame 0)))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :foo)))])

  (match (l/test-compile ::Main (let [x (::lang/ctor :foo)] [(::lang/call x) (::lang/call x)]))
    `[(r/cdef 0 [nil] [nil nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure (r/make-ctor ~'frame ::Main 1)))
          (r/define-call ~'frame 0 (r/node ~'frame 0))
          (r/define-call ~'frame 1 (r/node ~'frame 0))
          (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure clojure.core/vector))
            (r/join (r/call ~'frame 0))
            (r/join (r/call ~'frame 1)))))
      (r/cdef 0 [] [] nil
        (fn [~'frame] (r/pure :foo)))])

  (match (l/test-compile ::Main [(::lang/call (::lang/ctor :foo)) (::lang/call (::lang/ctor :bar))])
    `[(r/cdef 0 [] [nil nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/pure (r/make-ctor ~'frame ::Main 1)))
          (r/define-call ~'frame 1 (r/pure (r/make-ctor ~'frame ::Main 2)))
          (r/ap (r/lookup ~'frame :clojure.core/vector (r/pure clojure.core/vector))
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
          (r/define-call ~'frame 0 (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 1)
                                             (r/define-free 0 (r/node ~'frame 0)))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/pure (clojure.core/doto (r/make-ctor ~'frame ::Main 2)
                    (r/define-free 0 (r/free ~'frame 0))))))
      (r/cdef 1 [] [] nil
        (fn [~'frame]
          (r/free ~'frame 0)))]))

(tests "test-conditionals"
  ;; ({nil (ctor :y)} :x (ctor :z))
  (match (l/test-compile ::Main (case :x nil :y :z))
    `[(r/cdef 0 [] [nil] nil
        (fn [~'frame]
          (r/define-call ~'frame 0 (r/ap (r/ap (r/lookup ~'frame :clojure.core/hash-map (r/pure clojure.core/hash-map))
                                           (r/pure 'nil) (r/pure (r/make-ctor ~'frame ::Main 1)))
                                     (r/pure :x)
                                     (r/pure (r/make-ctor ~'frame ::Main 2))))
          (r/join (r/call ~'frame 0))))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :y)))
      (r/cdef 0 [] [] nil
        (fn [~'frame]
          (r/pure :z)))])

  (match (l/test-compile ::Main (case 'foo (foo bar) :share-this :else))
    `[(r/cdef 0 [nil] [nil] nil
        (fn [~'frame]
          (r/define-node ~'frame 0 (r/pure (r/make-ctor ~'frame ::Main 1)))
          (r/define-call ~'frame 0 (r/ap (r/ap (r/lookup ~'frame :clojure.core/hash-map (r/pure clojure.core/hash-map))
                                           (r/pure '~'foo) (r/node ~'frame 0)
                                           (r/pure '~'bar) (r/node ~'frame 0))
                                     (r/pure '~'foo)
                                     (r/pure (r/make-ctor ~'frame ::Main 2))))
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
          (r/define-node ~'frame 0 (r/pure (r/make-ctor ~'frame ::Main 1)))
          (r/define-call ~'frame 0 (r/ap (r/ap (r/lookup ~'frame :clojure.core/hash-map (r/pure clojure.core/hash-map))
                                           (r/pure 'nil) (r/node ~'frame 0)
                                           (r/pure 'false) (r/node ~'frame 0))
                                     (r/pure 1)
                                     (r/pure (r/make-ctor ~'frame ::Main 2))))
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

;; TODO test site is cleared on ctor boundary

;; TODO rewrite or remove
(comment
  (l/compile-client ((fn [] 1)))
  ;; ;; rest-args gensym breaks testability
  ;; ;; also, testing this deep is counter-productive, we're testing the implementation (internals)
  ;; := `(r/peer
  ;;       (lang/r-defs
  ;;         (lang/r-ap
  ;;           (lang/r-ap
  ;;             (lang/r-static
  ;;               (clojure.core/fn []
  ;;                 (clojure.core/fn [& rest-args32938]
  ;;                   (clojure.core/apply (fn* ([] 1)) rest-args32938)))))))
  ;;       [] 0)

  (l/compile-client (let [x 1] (fn [] x)))
  ;; := `(r/peer
  ;;       (lang/r-defs
  ;;         (lang/r-static 1)
  ;;         (lang/r-ap (lang/r-static
  ;;                      (clojure.core/fn [x32133]
  ;;                        (clojure.core/fn [& rest-args32134]
  ;;                          (clojure.core/let [x x32133]
  ;;                            (clojure.core/apply (fn* ([] x)) rest-args32134)))))
  ;;           (lang/r-local 0)))
  ;;       [] 1)
  )

(comment             ; TODO rewrite for new iteration
  ;; (<source-map-of-ap> <nil-for-prn> <nil-for-hello-world>)
  ;; source-map => ::line ::column
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
  )

;; cdef = definition of the static structure of an e/ctor
;; args :
;; * free variable count
;; * a vector of node sites
;; * a vector of call sites
;; * the result site
(comment

  ;; conditionals
  (l/compile ::Main `(case :x nil :y :z))
  := `[(r/cdef 0 [] [nil] nil
         (fn [frame]
           (r/define-call frame 0
             (r/ap
               (r/ap
                 (r/lookup frame :clojure.core/hash-map (r/pure hash-map))
                 (r/pure nil) (r/make-ctor frame ::Main 1))
               (r/pure :x)
               (r/pure (r/make-ctor frame ::Main 2))))
           (r/join (r/call frame 0))))
       (r/cdef 0 [] [] nil
         (fn [frame]
           (r/pure :y)))
       (r/cdef 0 [] [] nil
         (fn [frame]
           (r/pure :z)))]

  (l/compile ::Main (new (e/fn Foo [] (Foo.))))
  := `[(r/cdef 0 [] [nil] nil
         (fn [frame]
           (r/define-call frame 0
             (let [ctor (r/make-ctor frame ::Main 1)]
               (r/define-free ctor 0 (r/pure ctor))
               (r/pure ctor)))
           (r/join (r/call frame 0))))
       (r/cdef 1 [] [nil] nil
         (fn [frame]
           (r/define-call frame 0 (r/free frame 0))
           (r/join (r/call frame 0))))]

  (l/compile ::Main (e/letfn [(Foo [] (Bar.))
                              (Bar [] (Foo.))]
                      (Foo.)))
  := `[(r/cdef 0 [] [nil] nil
         (fn [frame]
           (let [ctor-foo (r/make-ctor frame ::Main 1)
                 ctor-bar (r/make-ctor frame ::Main 2)]
             (r/define-free ctor-foo 0 (r/pure ctor-foo))
             (r/define-free ctor-foo 1 (r/pure ctor-bar))
             (r/define-free ctor-bar 0 (r/pure ctor-bar))
             (r/define-free ctor-bar 1 (r/pure ctor-foo))
             (r/define-call frame 0 (r/pure ctor-foo))
             (r/join (r/call frame 0)))))
       (r/cdef 2 [] [nil] nil
         (fn [frame]
           (r/define-call frame 0 (r/free frame 1))
           (r/join (r/call frame 0))))
       (r/cdef 2 [] [nil] nil
         (fn [frame]
           (r/define-call frame 0 (r/free frame 1))
           (r/join (r/call frame 0))))]

  (l/compile ::Main (let [a :foo, b :bar, c :baz]
                      [(e/ctor [a b]) (e/ctor [b c])]))
  := `[(r/cdef 0 [nil nil nil] [] nil
         (fn [frame]
           (r/define-node frame 0 (r/pure :foo))
           (r/define-node frame 1 (r/pure :bar))
           (r/define-node frame 2 (r/pure :baz))
           (r/ap (r/lookup frame :clojure.core/vector (r/pure clojure.core/vector))
             (let [ctor (r/make-ctor frame ::Main 1)]
               (r/define-free ctor 0 (r/node frame 0))
               (r/define-free ctor 1 (r/node frame 1))
               (r/pure ctor))
             (let [ctor (r/make-ctor frame ::Main 2)]
               (r/define-free ctor 0 (r/node frame 1))
               (r/define-free ctor 1 (r/node frame 2))
               (r/pure ctor)))))
       (r/cdef 2 [] [] nil
         (fn [frame]
           (r/ap (r/lookup frame :clojure.core/vector (r/pure clojure.core/vector))
             (r/free frame 0)
             (r/free frame 1))))
       (r/cdef 2 [] [] nil
         (fn [frame]
           (r/ap (r/lookup frame :clojure.core/vector (r/pure clojure.core/vector))
             (r/free frame 0)
             (r/free frame 1))))]

  #_(e/defn Foo [])
  (l/compile ::Main `(e/ctor (e/call Foo))) :=
  `[(r/cdef 0 [] [] nil
      (fn [frame]
        (r/pure (r/make-ctor frame ::Main 1))))
    (r/cdef 0 [] [nil] nil
      (fn [frame]
        (r/define-call frame 0 (r/lookup frame ::Foo (r/pure (r/make-ctor frame ::Foo 0))))
        (r/join (r/call frame 0))))]

  (l/compile ::Main `e/frame) :=
  `[(r/cdef 0 [] [] nil
      (fn [frame]
        (r/pure frame)))]

  )
