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
        _ (check-arity-conflicts! ?name (mapv first positionals) (ffirst varargs))]
    ;; TODO map varargs
    `(check-electric fn
       ~(into (if-some [[args & body] (first varargs)]
                {-1 [(-> args count (- 2))
                     `(::lang/ctor
                        (let [~@(interleave (-> args pop pop) (map dget (range)))
                              ~(peek args) (dget -1)] ~@body))]} {})
          (map (cc/fn [[args & body]]
                 [(count args)
                  `(::lang/ctor
                     (let [~@(interleave args (map dget (range)))]
                       ~@body))])) positionals))))

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
        k (-> nm ns-qualify keyword)
        ctors (mapv #(lang/emit-ctor ts % env k) (lang/get-ordered-ctors-e ts))
        source `(cc/fn ([] ~(lang/emit-fn ts (lang/get-root-e ts) k))
                  ([idx#] (case idx# ~@(interleave (range) ctors))))
        deps (lang/emit-deps ts (lang/get-root-e ts))
        nm3 (vary-meta nm2 assoc ::lang/deps `'~deps)]
    (when-not (::lang/has-edef? (meta *ns*))
      (alter-meta! *ns* assoc ::lang/has-edef? true))
    (when (::lang/print-source env) (fipp.edn/pprint source))
    `(def ~nm3 ~source)))

(defmacro amb "
Syntax :
```clojure
(amb table1 table2 ,,, tableN)
```
Returns the concatenation of `table1 table2 ,,, tableN`.
" [& exprs] `(::lang/call (join (r/pure ~@(mapv #(list `ctor %) exprs)))))

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

(hyperfiddle.electric-de/defn Dispatch [F offset args]
  (let [arity (+ offset (count args))]
    (if-some [ctor (F arity)]
      (loop [offset offset
             args args
             ctor ctor]
        (if (< offset arity)
          (recur (inc offset) (next args)
            (r/bind ctor offset (::lang/pure (first args))))
          ctor))
      (let [[fixed ctor] (r/get-variadic F arity)]
        (loop [offset offset
               args args
               ctor ctor]
          (if (< offset fixed)
            (recur (inc offset) (next args)
              (r/bind ctor offset (::lang/pure (first args))))
            (r/bind ctor -1 (::lang/pure args))))))))

(hyperfiddle.electric-de/defn Apply
  ([F a]
   (::lang/call
     (r/bind-args ($ Dispatch F 0 a))))
  ([F a b]
   (::lang/call
     (r/bind-args ($ Dispatch F 1 b)
       (::lang/pure a))))
  ([F a b c]
   (::lang/call
     (r/bind-args ($ Dispatch F 2 c)
       (::lang/pure a) (::lang/pure b))))
  ([F a b c d]
   (::lang/call
     (r/bind-args ($ Dispatch F 3 d)
       (::lang/pure a) (::lang/pure b) (::lang/pure c))))
  ([F a b c d & es]
   (::lang/call
     (r/bind-args ($ Dispatch F (+ 4 (count es)) (concat (butlast es) (last es)))
       (::lang/pure a) (::lang/pure b) (::lang/pure c) (::lang/pure d)))))

(cc/defn on-unmount* [f] (m/observe (cc/fn [!] (! nil) f)))

(defmacro on-unmount "Run clojure(script) thunk `f` during unmount.

  Standard electric code runs on mount, therefore there is no `on-mount`."
  [f] `(input (on-unmount* ~f))) ; experimental
