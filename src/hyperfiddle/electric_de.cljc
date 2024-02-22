(ns hyperfiddle.electric-de
  (:refer-clojure :exclude [fn defn apply])
  (:require [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.incseq :as i]
            [clojure.core :as cc]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            #?(:clj [contrib.triple-store :as ts])
            #?(:clj [fipp.edn])
            [missionary.core :as m]
            [hyperfiddle.electric-local-def-de :as l])
  #?(:cljs (:require-macros hyperfiddle.electric-de)))

(defmacro ctor [expr] `(::lang/ctor ~expr))
(defmacro $ [F & args] `(binding [~@(interleave (range) args)] (::lang/call ~F)))

(defmacro pure "
Syntax :
```clojure
(pure table)
```
Returns the incremental sequence describing `table`.
" [expr] `(::lang/pure ~expr))

(defmacro join "
Syntax :
```clojure
(join incseq)
```
Returns the successive states of items described by `incseq`.
" [flow] `(::lang/join ~flow))

(defmacro check-electric [fn form]
  (if (::lang/electric &env)
    form
    (throw (ex-info (str "Electric code (" fn ") inside a Clojure function") (into {:electric-fn fn} (meta &form))))))

(defmacro fn [bs & body]
  `(check-electric fn
     (ctor
       (let [~@(interleave bs (eduction (map #(list ::lang/lookup %)) (range)))]
         ~@body))))

(cc/defn ns-qualify [sym] (if (namespace sym) sym (symbol (str *ns*) (str sym))))

#?(:clj (tests
          (ns-qualify 'foo) := `foo
          (ns-qualify 'a/b) := 'a/b))

(defmacro defn [nm bs & body]
  (let [env (merge (meta nm) (lang/normalize-env &env) l/web-config)
        expanded (lang/expand-all env `(fn ~bs ~@body))
        _ (when (::lang/print-expansion env) (fipp.edn/pprint expanded))
        ts (lang/analyze expanded '_ env (ts/->ts {::lang/->id (lang/->->id)}))
        ts (lang/analyze-electric env ts)
        ctors (mapv #(lang/emit-ctor ts % env (-> nm ns-qualify keyword)) (lang/get-ordered-ctors-e ts))
        deps (lang/emit-deps ts 0)
        nm (with-meta nm `{::lang/deps '~deps})]
    (when (::lang/print-source env) (fipp.edn/pprint ctors))
    `(def ~nm ~ctors)))

#_(defmacro defn [nm bs & body]
  ;; TODO cleanup env setup
  (let [env (merge (lang/normalize-env &env) l/web-config)
        ts (lang/analyze* env `(hyperfiddle.electric-de/fn ~bs ~@body))
        nm2 (vary-meta nm assoc ::lang/deps (lang/->deps ts))]
    `(def ~nm2 ~(lang/compile* ts))))

(defmacro amb "
Syntax :
```clojure
(amb table1 table2 ,,, tableN)
```
Returns the concatenation of `table1 table2 ,,, tableN`.
" [& exprs] `($ (join (r/pure ~@(mapv #(list `ctor %) exprs)))))

(defmacro input "
Syntax :
```clojure
(input cf)
```
Returns the current state of current continuous flow `cf`.
" [& flows] `(join (r/fixed-signals ~@flows)))

(defmacro watch "
Syntax :
```clojure
(watch !ref)
```
Returns the current state of current reference `!ref`.
" [ref] `(check-electric watch (input (m/watch ~ref))))

(defmacro diff-by "
Syntax :
```clojure
(diff-by kf xs)
```
Stabilizes successive states of collection `xs` with function `kf`. Returns each item as a table.
" [f xs] `(join (i/diff-by ~f (join (i/items (pure ~xs))))))

(defmacro drain "
Syntax :
```clojure
(drain expr)
```
Samples and discards `expr` synchronously with changes. Returns nothing.
" [expr] `(join (r/drain (pure ~expr))))

(defmacro client [& body] `(check-electric client (::lang/site :client ~@body)))
(defmacro server [& body] `(check-electric server (::lang/site :server ~@body)))

(defmacro cursor "
Syntax :
```clojure
(cursor [sym1 table1
         sym2 table2
         ,,,  ,,,
         symN tableN]
  & body)
```
For each tuple in the cartesian product of `table1 table2 ,,, tableN`, calls body in an implicit `do` with symbols
`sym1 sym2 ,,, symN` bound to the singleton tables for this tuple. Returns the concatenation of all body results.
" [bindings & body]
  (case bindings
    [] `(do ~@body)
    (let [[args exprs] (cc/apply map vector (partition-all 2 bindings))]
      `($ (r/bind-args (fn ~args ~@body)
               ~@(map (clojure.core/fn [expr]
                        `(r/fixed-signals (join (i/items (pure ~expr)))))
                   exprs))))))

(defmacro as-vec "
Syntax :
```clojure
(as-vec table)
```
Returns a single vector containing elements of `table`.
" [expr] `(input (m/reductions i/patch-vec (pure ~expr))))

(defmacro for-by "
Syntax :
```clojure
(for-by kf [sym1 coll1
            sym2 coll2
            ,,,  ,,,
            symN collN]
  & body)
```
Stabilizes successives states of `coll1 coll2 ,,, collN` with function `kf`. For each tuple in the cartesian product of
resulting tables, calls body in an implicit `do` with symbols `sym1 sym2 ,,, symN` bound to the singleton tables for
this tuple. Returns the concatenation of all body results as a single vector.
" [kf bindings & body]
  `(as-vec
     ~((clojure.core/fn rec [bindings]
         (if-some [[sym expr & bindings] bindings]
           `(cursor [~sym (diff-by ~kf ~expr)]
              ~(rec bindings)) `(do ~@body)))
       (seq bindings))))

(cc/defn- -splicev [args] (into [] cat [(pop args) (peek args)]))
(hyperfiddle.electric-de/defn ^::lang/static-vars Apply* [F args]
  (let [spliced (-splicev args)]
    (case (count spliced)
      0 ($ F)
      1 ($ F (nth spliced 0))
      2 ($ F (nth spliced 0) (nth spliced 1))
      3 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2))
      4 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3))
      5 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4))
      6 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5))
      7 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6))
      8 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7))
      9 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8))
      10 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9))
      11 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10))
      12 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11))
      13 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12))
      14 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13))
      15 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14))
      16 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15))
      17 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15) (nth spliced 16))
      18 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15) (nth spliced 16) (nth spliced 17))
      19 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15) (nth spliced 16) (nth spliced 17) (nth spliced 18))
      20 ($ F (nth spliced 0) (nth spliced 1) (nth spliced 2) (nth spliced 3) (nth spliced 4) (nth spliced 5) (nth spliced 6) (nth spliced 7) (nth spliced 8) (nth spliced 9) (nth spliced 10) (nth spliced 11) (nth spliced 12) (nth spliced 13) (nth spliced 14) (nth spliced 15) (nth spliced 16) (nth spliced 17) (nth spliced 18) (nth spliced 19)))))
(defmacro apply [F & args] `($ Apply* ~F [~@args]))
