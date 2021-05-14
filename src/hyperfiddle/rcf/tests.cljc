(ns hyperfiddle.rcf.tests
  #?(:cljs (:require[hyperfiddle.rcf :as rcf]))
  #?(:clj (:require [hyperfiddle.rcf :as rcf :refer [examples]]
                    [clojure.test :as t])
     :cljs (:require-macros [hyperfiddle.rcf :as rcf :refer [examples]])))

(examples
  "Syntax"
  (examples
    "an infix assertion is converted to a prefix one"
    (rcf/rewrite-infix '(1 2 3)) := '((do 1) (do 2) (do 3))
    (rcf/rewrite-infix '(1 := 2)) := '((= 1 2))
    (rcf/rewrite-infix '(0 1 := 2 3)) := '((do 0) (= 1 2) (do 3))))

(examples "Usage:"
  (examples "infix `:=` is equality assertion"
    1 := 1
    "foo" := "foo"
    (inc 0) := (dec 2))
  (examples "a prefix expression works as in Clojure"
    (:= 1 1)  ; useful to eval at the repl
    (:= 1 1)) ; works too

  (examples "unification is supported with `:?=`"
    1 :?= '?a
    {:a 1, :b 2} :?= '{:a ?a, :b ?b}    ; passes {?a 1, ?b 2}
    (examples "this fails because 1 != 2"
      (rcf/unifies? {:a 1, :b 2} '{:a ?a, :b ?a}) := false
      (examples "and works as infix too"
        (:?= 1 '?a)))
    (examples "wildcard is supported with `_` and always unifies."
      {:a 1, :b 2} :?= '{:a _, :b _})))

(examples "*1, *2 and *3 are respectively bound to the last, penultimate and antepenultimate values."
  :foo
  :bar
  :baz
  *3 := :foo
  *2 := :bar
  *1 := :baz)

(rcf/with-config {:dots true}
  (examples "Theses will just print a single char: ✅ if they succeed"
    (:= 1 1)
    (:= 1 1)))

;;;;;;;;;;;;;;;;;;;;;;
;; RUNNING FLOW CLI ;;
;;;;;;;;;;;;;;;;;;;;;;

#?(:clj
   (defn -main [& _args]
     (let [{:keys [enabled generate-tests]} rcf/*config*]
       (when enabled
         (println "\n\nTests should run only when the -Dhyperfiddle.rcf.enabled=\"true\" JVM property is present. \n"))

       (when generate-tests
         (println "\n\nClojure.test test runner should see generated tests when the -Dhyperfiddle.rcf.generate-tests=\"true\" JVM property is present \n")
         (t/run-all-tests #"hyperfiddle.rcf.tests"))

       (when (and (not enabled) (not generate-tests))
         (println "\nHyperfiddle.rcf is disabled, `(examples …)` blocks are treated as comments. \n")))))

#_(examples
    "Pattern matching works as in core.match, using :matches?"
    ;; '{:a 1, :b 2} :matches? {:a _, :b _}
    ;; '(:a :b :c)   :matches? ([:a _ :c] :seq)
    "Hello" :matches? #"H.*"

    (examples
      "it also works in prefix position"
      (:matches? 1 1)
      (:matches? "foo" #".oo")))
