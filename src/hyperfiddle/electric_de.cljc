(ns hyperfiddle.electric-de
  (:refer-clojure :exclude [fn])
  (:require [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.incseq :as i]
            [missionary.core :as m]
            [hyperfiddle.electric-local-def-de :as l])
  #?(:cljs (:require-macros hyperfiddle.electric-de)))

(defmacro join [flow] `(::lang/join ~flow))
(defmacro input [flow] `(join (i/fixed ~flow)))
(defmacro watch [ref] `(input (m/watch ~ref)))
(defmacro ctor [expr] `(::lang/ctor ~expr))
(defmacro call [ctor] `(::lang/call ~ctor))
(defmacro pure [v] `(::lang/pure ~v))
(defmacro amb [& exprs] `(call (join (r/pure ~@(mapv #(list `ctor %) exprs)))))
(defmacro fn [bs & body]
  `(ctor
    (let [~@(interleave bs (eduction (map #(list ::lang/lookup %)) (range)))]
      ~@body)))
(defmacro cursor [[sym v] & body] `(call (r/bind-args (fn [~sym] ~@body) (join (r/singletons (pure ~v))))))
(defmacro diff-by [f xs] `(join (i/diff-by ~f (join (i/items (pure ~xs))))))
;; (defmacro drain [expr] `(join (i/drain (pure ~expr))))
(defmacro client [& body] `(::lang/site :client ~@body))
(defmacro server [& body] `(::lang/site :server ~@body))
