(ns dustin.analyzer
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.jvm :as clj]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.passes.jvm.emit-form :as e]
            [hyperfiddle.rcf :refer [tests]]))

; Docs:
; https://github.com/clojure/tools.analyzer

(tests
  (clj/analyze '(inc 1))
  := '{:args       [{:op       :const,
                     :env      {:context :ctx/expr, :locals {}, :ns dustin.analyzer, :column _, :line _, :file _},
                     :type     :number,
                     :literal? true,
                     :val      1,
                     :form     1,
                     :o-tag    long,
                     :tag      long}],
       :children   [:args],
       :method     inc,
       :op         :static-call,
       :env        {:context :ctx/expr, :locals {}, :ns dustin.analyzer, :column _, :line _, :file _},
       :o-tag      long,
       :class      clojure.lang.Numbers,
       :top-level  true,
       :form       (. clojure.lang.Numbers (inc 1)),
       :tag        long,
       :validated? true,
       :raw-forms  ((inc 1))})

(tests
  (ast/children (clj/analyze '(inc 1)))
  := [{:op       :const,
       :env      {:context :ctx/expr, :locals {}, :ns _, :file _},
       :type     :number,
       :literal? true,
       :val      1,
       :form     1,
       :o-tag    _,
       :tag      _}])

(tests
  "flatten"
  (ast/nodes (clj/analyze '(inc 1)))
  := [{:op         :static-call,
       :method     'inc,
       :form       '(. clojure.lang.Numbers (inc 1)),
       :raw-forms  '((inc 1))
       :args       [{:op       :const,
                     :env      _,
                     :type     :number,
                     :literal? true,
                     :val      1,
                     :form     1,
                     :o-tag    long,
                     :tag      long}],
       :children   [:args],
       :env        _,
       :o-tag      long,
       :class      clojure.lang.Numbers,
       :top-level  true,
       :tag        long,
       :validated? true}
      {:op       :const,
       :form     1,
       :val      1,
       :env      _,
       :type     :number,
       :literal? true,
       :o-tag    long,
       :tag      long}])

(tests
  (e/emit-form (clj/analyze '(inc 1)))
  := '(clojure.lang.Numbers/inc 1))

(tests
  (clj/analyze '(if true :a :b))
  := {:form      '(if true :a :b),
      :op        :if,
      :children  [:test :then :else],
      :else      {:op       :const,
                  :env      _,
                  :type     :keyword,
                  :literal? true,
                  :val      :b,
                  :form     :b,
                  :o-tag    clojure.lang.Keyword,
                  :tag      clojure.lang.Keyword}
      :then      {:op       :const,
                  :env      _,
                  :type     :keyword,
                  :literal? true,
                  :val      :a,
                  :form     :a,
                  :o-tag    clojure.lang.Keyword,
                  :tag      clojure.lang.Keyword}
      :test      {:op       :const,
                  :env      _,
                  :type     :bool,
                  :literal? true,
                  :val      true,
                  :form     true,
                  :o-tag    java.lang.Boolean,
                  :tag      java.lang.Boolean}
      :env       _,
      :o-tag     clojure.lang.Keyword,
      :top-level true,
      :tag       clojure.lang.Keyword})

(tests
  (clj/analyze '(let [a 1]))
  := _)

(tests
  (def body '(loop [x 0]
               (case (int x)
                 0 (recur (inc x))
                 1 42)))
  (clj/analyze body)
  := _

  (e/emit-form (clj/analyze body))
  := '(loop* [x 0]
        (let* [?G__40534 (clojure.lang.RT/intCast x)]
          ; https://github.com/clojure/clojure/blob/a29f9b911b569b0a4890f320ec8f946329bbe0fd/src/jvm/clojure/lang/Compiler.java#L9006
          (case* ?G__40534
            0 0
            (throw (new java.lang.IllegalArgumentException (clojure.core/str "No matching clause: " ?G__40534)))
            {0 [0 (recur (clojure.lang.Numbers/inc x))],
             1 [1 42]}
            :compact
            :int
            nil))))

(tests
  (clj/empty-env)
  := {:context :ctx/expr, :locals {}, :ns 'dustin.analyzer}
  )

(defmacro foo [x] ::x)
(apply #'foo '(foo 1) {} [1])
