(ns hyperfiddle.electric-de
  (:refer-clojure :exclude [fn])
  (:require [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.incseq :as i]
            [missionary.core :as m]
            [hyperfiddle.electric-local-def-de :as l])
  #?(:cljs (:require-macros hyperfiddle.electric-de)))

(defmacro join [flow] `(::lang/join ~flow))
(defmacro input [& flows] `(join (r/fixed-signals ~@flows)))
(defmacro watch [ref] `(input (m/watch ~ref)))
(defmacro ctor [expr] `(::lang/ctor ~expr))
(defmacro call [ctor] `(::lang/call ~ctor))
(defmacro pure [v] `(::lang/pure ~v))
(defmacro amb [& exprs] `(call (join (r/pure ~@(mapv #(list `ctor %) exprs)))))
(defmacro fn [bs & body]
  `(ctor
    (let [~@(interleave bs (eduction (map #(list ::lang/lookup %)) (range)))]
      ~@body)))

(defmacro diff-by "
Syntax :
```clojure
(diff-by kf xs)
```
Stabilizes successive states of collection `xs` with function `kf`. Returns each item as a table.
" [f xs] `(join (i/diff-by ~f (join (i/items (pure ~xs))))))

;; (defmacro drain [expr] `(join (i/drain (pure ~expr))))
(defmacro client [& body] `(::lang/site :client ~@body))
(defmacro server [& body] `(::lang/site :server ~@body))

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
    (let [[args exprs] (apply map vector (partition-all 2 bindings))]
      `(call (r/bind-args (fn ~args ~@body)
               ~@(map (clojure.core/fn [expr]
                        `(r/fixed-signals (join (i/items (pure ~expr)))))
                   exprs))))))