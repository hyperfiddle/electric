(ns hyperfiddle.electric.impl.cljs-analyzer2-test
  (:require [clojure.test :as t]
            [cljs.env]
            [cljs.analyzer]
            [hyperfiddle.electric.impl.cljs-analyzer2 :as ana]))

(comment
  (time (let [!a (atom {})] (ana/analyze-nsT !a {} 'cljs.core)))
  (-> @!a ::ana/nses (get 'cljs.core) ::ana/defs count)
  )

(t/deftest ns-expansion
  (let [ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze
        !a (ana/->!a)
        _ (ana/analyze-nsT !a {} ns$)
        a @!a]
    (t/is (nil? (ana/find-var !a 'non ns$ {})))
    (t/is (nil? (ana/find-var !a 'first ns$ {})))
    (t/is (= 'cljs.core/next (::ana/name (ana/find-var !a 'nxt ns$ {}))))
    (t/are [x] (some? (ana/find-var !a x ns$ {}))
      'foo
      'bar
      'baz
      'an-fn
      'behind-require
      'str
      'behind-alias
      'behind-require-macros
      'behind-require-macro-alias
      'behind-required-refer
      'behind-required-rename
      'behind-require-macro-refer
      'behind-require-macro-rename
      'behind-include-macros
      'behind-refer-macros
      'behind-use
      'behind-use-renamed
      'behind-use-macro
      'behind-use-macro-renamed
      'behind-auto-alias
      'behind-auto-alias-alias
      'behind-auto-alias-refer
      'nxt)))

(t/deftest runtime-vars
  (let [ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze,
        !a (ana/->!a)
        _ (ana/analyze-nsT !a {} ns$)
        a @!a]
    (t/are [x] (nil? (ana/find-var !a x ns$ {}))
      'hyperfiddle.electric.impl.cljs-file-to-analyze.runtime/only-macro
      'run/only-macro
      'only-macro
      'next) ; renamed in :refer-clojure
    (t/are [x] (some? (ana/find-var !a x ns$ {}))
      'hyperfiddle.electric.impl.cljs-file-to-analyze.runtime/macro-and-runtime
      'run/macro-and-runtime
      'macro-and-runtime
      'hyperfiddle.electric.impl.cljs-file-to-analyze.runtime/only-runtime
      'run/only-runtime
      'only-runtime)))

(t/deftest local-shadowing
  (let [ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze
        !a (ana/->!a)
        _ (ana/analyze-nsT !a {} ns$)
        a @!a]
    (t/are [x] (nil? (ana/find-var !a x ns$ {}))
      'shadowed-by-let
      'shadowed-by-let-destructure
      'shadowed-by-fn
      'shadowed-by-fn-destructure
      'shadowed-by-letfn-fn-name
      'shadowed-by-letfn-other-fn-name
      'shadowed-by-letfn-local)))

(t/deftest defs-match-official-cljs-analyzer
  (let [ns$ 'cljs.analyzer
        !a (ana/->!a)
        _ (ana/analyze-nsT !a {} ns$)
        a @!a
        c (cljs.env/ensure
            (cljs.analyzer/analyze-file "cljs/core.cljs")
            (cljs.analyzer/analyze-file "cljs/analyzer.cljc")
            @cljs.env/*compiler*)]
    (t/are [ns$] (= (into #{} (keep (fn [[k v]] (when-not (:anonymous v) k)))
                      (-> c :cljs.analyzer/namespaces (get ns$) :defs))
                   (set (-> a ::ana/nses (get ns$) ::ana/defs keys)))
      'cljs.core
      'cljs.analyzer)))

(t/deftest clojure-core-var-found-as-cljs-core-var
  (let [ns$ 'cljs.analyzer
        !a (ana/->!a)
        _ (ana/analyze-nsT !a {} ns$)
        a @!a]
    (t/is (some? (ana/find-var !a 'clojure.core/vector ns$ {})))))

(t/deftest non-required-var-can-be-found ; e.g. a macro from another ns might have expanded to it
  (let [ns$ 'cljs.source-map
        !a (ana/->!a)
        _ (ana/analyze-nsT !a {} ns$)
        a @!a]
    (t/is (some? (ana/find-var !a 'cljs.source-map/encode 'cljs.core {})))))

(t/deftest npm-shadow-extension
  (let [ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze
        !a (ana/->!a)
        _ (ana/analyze-nsT !a {} ns$)
        a @!a]
    (t/is (boolean (ana/js-call? a 'jslib/foo ns$)))
    (t/is (boolean (ana/js-call? a 'js/alert ns$)))
    (t/is (boolean (ana/js-call? a 'js-referred ns$)))
    (t/is (boolean (ana/js-call? a 'js-renamed ns$)))
    (t/is (not (ana/js-call? a 'not-js-referred ns$)))
    (t/is (not (ana/js-call? a 'run/only-runtime ns$)))))

(t/deftest lazy-demand-analysis
  (let [ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze
        runtime-ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze.runtime
        !a (ana/->!a)
        _ (ana/analyze-ns-structureT !a {} ns$)]
    ;; Structure is populated for the test ns
    (t/is (= runtime-ns$ (-> @!a ::ana/nses (get ns$) ::ana/requires (get 'run)))
      "alias 'run' resolves to runtime ns after structure analysis")
    ;; Dependency NOT fully analyzed yet
    (t/is (nil? (-> @!a ::ana/nses (get runtime-ns$) ::ana/defs))
      "dependency ::defs should be nil after structure-only analysis")
    (t/is (nil? (-> @!a ::ana/ns-tasks (get runtime-ns$)))
      "dependency ::ns-tasks should not be set after structure-only analysis")
    ;; find-var for qualified alias triggers demand analysis
    (t/is (some? (ana/find-var !a 'run/only-runtime ns$ {}))
      "find-var with alias should trigger demand analysis")
    (t/is (true? (-> @!a ::ana/ns-tasks (get runtime-ns$)))
      "::ns-tasks should be set after demand-triggered analysis")
    ;; find-var for referred symbol triggers demand analysis
    (let [!a2 (ana/->!a)
          _ (ana/analyze-ns-structureT !a2 {} ns$)]
      (t/is (some? (ana/find-var !a2 'only-runtime ns$ {}))
        "find-var for referred symbol should trigger demand analysis")
      (t/is (true? (-> @!a2 ::ana/ns-tasks (get runtime-ns$)))
        "::ns-tasks set after referred symbol demand analysis"))
    ;; find-var for fully-qualified symbol triggers demand analysis
    (let [!a3 (ana/->!a)
          _ (ana/analyze-ns-structureT !a3 {} ns$)]
      (t/is (some? (ana/find-var !a3 (symbol (str runtime-ns$) "only-runtime") ns$ {}))
        "find-var for fully-qualified symbol should trigger demand analysis")
      (t/is (true? (-> @!a3 ::ana/ns-tasks (get runtime-ns$)))
        "::ns-tasks set after fully-qualified demand analysis"))))

(t/deftest structure-tasks-set-by-full-analysis
  (let [ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze
        !a (ana/->!a)
        _ (ana/analyze-nsT !a {} ns$)]
    (t/is (true? (-> @!a ::ana/ns-tasks (get ns$)))
      "::ns-tasks should be set after full analysis")
    (t/is (true? (-> @!a ::ana/ns-structure-tasks (get ns$)))
      "::ns-structure-tasks should also be set after full analysis")))

(t/deftest imports
  (let [a (ana/add-import (atom {}) 'foo '[Foo [java.util X Y]])]
    (t/is (= '{::ana/nses {foo {::ana/imports #{java.util.X X Foo java.util.Y Y}}}} a))
    (t/is (some? (ana/imported? a 'X.XXX 'foo)))
    (t/is (some? (ana/imported? a 'java.util.X.XXX 'foo)))
    (t/is (some? (ana/imported? a 'Foo.Bar 'foo)))))
