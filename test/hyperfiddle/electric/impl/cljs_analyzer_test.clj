(ns hyperfiddle.electric.impl.cljs-analyzer-test
  (:require [clojure.test :as t]
            [cljs.env]
            [cljs.analyzer]
            [hyperfiddle.electric.impl.cljs-analyzer :as ana]))

;; (comment
;;   (def a (ana/analyze-ns 'hyperfiddle.electric.impl.cljs-file-to-analyze))
;;   (-> a ::ana/nses (get 'hyperfiddle.electric.impl.cljs-file-to-analyze) ::ana/refers)
;;   (ana/find-var a 'next 'hyperfiddle.electric.impl.cljs-file-to-analyze)
;;   (ana/expand (ana/->a) 'cljs.core #{} '(defmacro macrodef [sym] `(def ~sym)))
;;   (-> (ana/collect-defs (ana/->a) 'foo '(defmacro macrodef [sym] `(def ~sym)))
;;     ::ana/nses (get 'foo))
;;   (-> (ana/collect-defs (ana/->a) 'foo '(fn [] (def x 1)))
;;     ::ana/nses (get 'foo))
;;   )

;; (t/deftest ns-expansion
;;   (let [ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze
;;         a (ana/analyze-ns ns$)]
;;     (t/is (nil? (ana/find-var a 'non ns$)))
;;     (t/is (nil? (ana/find-var a 'first ns$)))
;;     (t/is (= 'cljs.core/next (::ana/name (ana/find-var a 'nxt ns$))))
;;     (t/are [x] (some? (ana/find-var a x ns$))
;;       'foo
;;       'bar
;;       'baz
;;       'an-fn
;;       'behind-require
;;       'str
;;       'behind-alias
;;       'behind-require-macros
;;       'behind-require-macro-alias
;;       'behind-required-refer
;;       'behind-required-rename
;;       'behind-require-macro-refer
;;       'behind-require-macro-rename
;;       'behind-include-macros
;;       'behind-refer-macros
;;       'behind-use
;;       'behind-use-renamed
;;       'behind-use-macro
;;       'behind-use-macro-renamed
;;       'behind-auto-alias
;;       'behind-auto-alias-alias
;;       'behind-auto-alias-refer
;;       'nxt)))

;; (t/deftest runtime-vars
;;   (let [ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze,
;;         a (ana/analyze-ns ns$)]
;;     (t/are [x] (nil? (ana/find-var a x ns$))
;;       'hyperfiddle.electric.impl.cljs-file-to-analyze.runtime/only-macro
;;       'run/only-macro
;;       'only-macro
;;       'next) ; renamed in :refer-clojure
;;     (t/are [x] (some? (ana/find-var a x ns$))
;;       'hyperfiddle.electric.impl.cljs-file-to-analyze.runtime/macro-and-runtime
;;       'run/macro-and-runtime
;;       'macro-and-runtime
;;       'hyperfiddle.electric.impl.cljs-file-to-analyze.runtime/only-runtime
;;       'run/only-runtime
;;       'only-runtime)))

;; (t/deftest local-shadowing
;;   (let [ns$ 'hyperfiddle.electric.impl.cljs-file-to-analyze
;;         a (ana/analyze-ns ns$)]
;;     (t/are [x] (nil? (ana/find-var a x ns$))
;;       'shadowed-by-let
;;       'shadowed-by-let-destructure
;;       'shadowed-by-fn
;;       'shadowed-by-fn-destructure
;;       'shadowed-by-letfn-fn-name
;;       'shadowed-by-letfn-other-fn-name
;;       'shadowed-by-letfn-local)))

;; (t/deftest defs-match-official-cljs-analyzer
;;   (let [ns$ 'cljs.analyzer
;;         a (ana/analyze-ns ns$)
;;         c (cljs.env/ensure
;;             (cljs.analyzer/analyze-file "cljs/core.cljs")
;;             (cljs.analyzer/analyze-file "cljs/analyzer.cljc")
;;             @cljs.env/*compiler*)]
;;     (t/are [ns$] (= (into #{} (keep (fn [[k v]] (when-not (:anonymous v) k)))
;;                       (-> c :cljs.analyzer/namespaces (get ns$) :defs))
;;                    (set (-> a ::ana/nses (get ns$) ::ana/defs keys)))
;;       'cljs.core
;;       'cljs.analyzer)))
