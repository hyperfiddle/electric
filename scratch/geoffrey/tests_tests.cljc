(ns geoffrey.tests-tests
  #?(:cljs (:require[geoffrey.tests :as t]))
  #?(:clj (:require [geoffrey.tests :as t :refer [tests]])
     :cljs (:require-macros [geoffrey.tests :as t :refer [tests]])))

(tests
 "Syntax"
 (tests
  "an infix assertion is converted to a prefix one"
  (t/rewrite-infix '(1 2 3)) := '((do 1) (do 2) (do 3))
  (t/rewrite-infix '(1 := 2)) := '((= 1 2))
  (t/rewrite-infix '(0 1 := 2 3)) := '((do 0) (= 1 2) (do 3))))

(tests "Usage:"
  (tests "infix `:=` is equality assertion"
    1 := 1
    "foo" := "foo"
    (inc 0) := (dec 2))
  (tests "a prefix expression works as in Clojure"
    (:= 1 1)  ; useful to eval at the repl
    (:= 1 1)) ; works too

  (tests "unification is supported with `:?=`"
    1 :?= '?a
    {:a 1, :b 2} :?= '{:a ?a, :b ?b}    ; passes {?a 1, ?b 2}
    (tests "this fails because 1 != 2"
      (t/unifies? {:a 1, :b 2} '{:a ?a, :b ?a}) := false
      (tests "and works as infix too"
        (:?= 1 '?a)))
    (tests "wildcard is supported with `_` and always unifies."
      {:a 1, :b 2} :?= '{:a _, :b _})))

(tests "*1, *2 and *3 are respectively bound to the last, penultimate and antepenultimate values."
  :foo
  :bar
  :baz
  *3 := :foo
  *2 := :bar
  *1 := :baz)

(t/with-config {:dots true}
  (tests "Theses will just print a single char: ✅ if they succeed"
    (:= 1 1)
    (:= 1 1)))



#_(tests
   "Pattern matching works as in core.match, using :matches?"
   ;; '{:a 1, :b 2} :matches? {:a _, :b _}
   ;; '(:a :b :c)   :matches? ([:a _ :c] :seq)
   "Hello" :matches? #"H.*"

   (tests
    "it also works in prefix position"
    (:matches? 1 1)
    (:matches? "foo" #".oo")))
