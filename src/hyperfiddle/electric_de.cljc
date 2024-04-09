(ns hyperfiddle.electric-de
  (:refer-clojure :exclude [fn defn apply letfn])
  (:require [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.incseq :as i]
            [clojure.core :as cc]
            [clojure.string :as str]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            #?(:clj [contrib.triple-store :as ts])
            #?(:clj [fipp.edn])
            [missionary.core :as m]
            [hyperfiddle.electric-local-def-de :as l])
  #?(:cljs (:require-macros hyperfiddle.electric-de)))

#?(:clj (cc/defn dget [v] `(::lang/lookup ~v)))
#?(:clj (cc/defn ->pos-args [n] (eduction (take n) (map dget) (range))))

(defmacro check-electric [fn form]
  (if (::lang/electric &env)
    form
    (throw (ex-info (str "Electric code (" fn ") inside a Clojure function") (into {:electric-fn fn} (meta &form))))))

(defmacro ctor [expr] `(::lang/ctor ~expr))
(defmacro $ [F & args] `(check-electric $ (lang/$ ~F ~@args)))

(defmacro frame []
  `(::lang/pure (::lang/frame)))

(defmacro pure "
Syntax :
```clojure
(pure table)
```
Returns the incremental sequence describing `table`.
" [expr] `(r/incseq (frame) (::lang/pure ~expr)))

(defmacro join "
Syntax :
```clojure
(join incseq)
```
Returns the successive states of items described by `incseq`.
" [flow] `(::lang/join ~flow))

(defmacro fn* [bs & body]
  `(check-electric fn*
     (ctor
       (let [~@(interleave bs (->pos-args (count bs)))]
         ~@body))))

#?(:clj (cc/defn- varargs? [args] (boolean (and (seq args) (= '& (-> args pop peek))))))

#?(:clj (cc/defn- ?bind-self [code ?name] (cond->> code ?name (list 'let* [?name `(::lang/lookup ::r/fn)]))))

(cc/defn -prep-varargs [n argv map-vararg?]
  (let [v (into [] (drop n) argv)]
    (when (seq v)                 ; varargs value is `nil` when no args provided
      (if map-vararg?             ; [x y & {:keys [z]}] <- vararg map destructuring
        (if (even? (count v))
          (cc/apply array-map v)                          ; ($ MapVararg :x 1)
          (merge (cc/apply array-map (pop v)) (peek v))) ; ($ MapVararg :x 1 {:y 2})
        v))))

#?(:clj (cc/defn- throw-arity-conflict! [?name group]
          (throw (ex-info (str "Conflicting arity definitions" (when ?name (str " in " ?name)) ": "
                            (str/join " and " group))
                   {:name ?name}))))

#?(:clj (cc/defn- check-only-one-vararg! [?name varargs]
          (when (> (count varargs) 1)
            (throw-arity-conflict! ?name varargs))))

#?(:clj (cc/defn- check-arity-conflicts! [?name positionals vararg]
          (let [grouped (group-by count positionals)]
            (doseq [[_ group] grouped]
              (when (> (count group) 1)
                (throw-arity-conflict! ?name group)))
            (when-some [same (get grouped (-> vararg count dec))]
              (throw-arity-conflict! ?name (conj same vararg))))))

(defmacro fn [& args]
  (let [[?name args2] (if (symbol? (first args)) [(first args) (rest args)] [nil args])
        arities (cond-> args2 (vector? (first args2)) list)
        {positionals false, varargs true} (group-by (comp varargs? first) arities)
        _ (check-only-one-vararg! ?name (mapv first varargs))
        _ (check-arity-conflicts! ?name (mapv first positionals) (ffirst varargs))
        [?vararg npos map-vararg?] (when-some [va (first varargs)]
                                     (let [[args & body] va
                                           npos (-> args count (- 2))
                                           unvarargd (-> args pop pop (conj (peek args)))]
                                       `[(hyperfiddle.electric-de/fn* ~unvarargd ~@body) ~npos ~(map? (peek args))]))
        dispatch-map (into {} (map (cc/fn [[args :as fargs]] [(count args) `(hyperfiddle.electric-de/fn* ~@fargs)]))
                       positionals)]
    `(check-electric fn
       (ctor
         ~(-> (if ?vararg
                (let [code `(binding [~npos (-prep-varargs ~npos (::lang/lookup ::r/argv) ~map-vararg?)] (::lang/call ~?vararg))]
                  (if (seq positionals)
                    `(if-some [F# (~dispatch-map (::lang/lookup ::r/arity))] (::lang/call F#) ~code)
                    code))
                `(::lang/call (~dispatch-map (::lang/lookup ::r/arity))))
            (?bind-self ?name))))))

(cc/defn ns-qualify [sym] (if (namespace sym) sym (symbol (str *ns*) (str sym))))

#?(:clj (tests
          (ns-qualify 'foo) := `foo
          (ns-qualify 'a/b) := 'a/b))

(defmacro defn [nm & fdecl]
  (let [[_defn sym] (macroexpand `(cc/defn ~nm ~@fdecl))
        env (merge (meta nm) (lang/normalize-env &env) l/web-config {::lang/def nm})
        nm2 (vary-meta nm merge (meta sym))
        expanded (lang/expand-all env `(fn ~nm2 ~@(cond-> fdecl (string? (first fdecl)) next)))
        _ (when (::lang/print-expansion env) (fipp.edn/pprint expanded))
        ts (lang/analyze expanded '_ env (lang/->ts))
        ts (lang/analyze-electric env ts)
        ctors (mapv #(lang/emit-ctor ts % env (-> nm ns-qualify keyword)) (lang/get-ordered-ctors-e ts))
        deps (lang/emit-deps ts (lang/get-root-e ts))
        nm3 (vary-meta nm2 assoc ::lang/deps `'~deps)]
    (when-not (::lang/has-edef? (meta *ns*))
      (alter-meta! *ns* assoc ::lang/has-edef? true))
    (when (::lang/print-source env) (fipp.edn/pprint ctors))
    `(def ~nm3 ~ctors)))

(defmacro amb "
Syntax :
```clojure
(amb table1 table2 ,,, tableN)
```
Returns the concatenation of `table1 table2 ,,, tableN`.
" [& exprs] `(::lang/call (join (i/fixed ~@(map #(list `r/invariant (list `ctor %)) exprs)))))

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
      `(::lang/call
        (r/bind-args (hyperfiddle.electric-de/fn* ~args ~@body)
          ~@(map (clojure.core/fn [expr]
                   `(r/effect (r/fixed-signals (join (i/items (pure ~expr))))))
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

(defmacro letfn [bs & body]
  (let [sb (reverse bs)]
    (reduce (cc/fn [ac [nm]] `(::lang/mklocal ~nm ~ac))
      (reduce (cc/fn [ac [nm & fargs]] `(::lang/bindlocal ~nm (hyperfiddle.electric-de/fn ~@fargs) ~ac)) (cons 'do body) sb)
      sb)))

(cc/defn- -splicev [args] (into [] cat [(pop args) (peek args)]))
(hyperfiddle.electric-de/defn Apply* [F args]
  (let [s (-splicev args)]
    (case (count s)
      0 ($ F)
      1 ($ F (nth s 0))
      2 ($ F (nth s 0) (nth s 1))
      3 ($ F (nth s 0) (nth s 1) (nth s 2))
      4 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3))
      5 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4))
      6 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5))
      7 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6))
      8 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7))
      9 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8))
      10 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9))
      11 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10))
      12 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10) (nth s 11))
      13 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10) (nth s 11) (nth s 12))
      14 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10) (nth s 11) (nth s 12) (nth s 13))
      15 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10) (nth s 11) (nth s 12) (nth s 13) (nth s 14))
      16 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10) (nth s 11) (nth s 12) (nth s 13) (nth s 14) (nth s 15))
      17 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10) (nth s 11) (nth s 12) (nth s 13) (nth s 14) (nth s 15) (nth s 16))
      18 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10) (nth s 11) (nth s 12) (nth s 13) (nth s 14) (nth s 15) (nth s 16) (nth s 17))
      19 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10) (nth s 11) (nth s 12) (nth s 13) (nth s 14) (nth s 15) (nth s 16) (nth s 17) (nth s 18))
      20 ($ F (nth s 0) (nth s 1) (nth s 2) (nth s 3) (nth s 4) (nth s 5) (nth s 6) (nth s 7) (nth s 8) (nth s 9) (nth s 10) (nth s 11) (nth s 12) (nth s 13) (nth s 14) (nth s 15) (nth s 16) (nth s 17) (nth s 18) (nth s 19)))))
(defmacro apply [F & args] `($ Apply* ~F [~@args]))

(cc/defn on-unmount* [f] (m/observe (cc/fn [!] (! nil) f)))

(defmacro on-unmount "Run clojure(script) thunk `f` during unmount.

  Standard electric code runs on mount, therefore there is no `on-mount`."
  [f] `(input (on-unmount* ~f))) ; experimental
