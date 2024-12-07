(ns contrib.clojurex
  #?(:cljs (:require-macros contrib.clojurex))
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn pyramid [op [s expr & more :as bindings] body]
  (if (seq more)
    `(~'binding [~s ~expr] ~(pyramid op more body))
    `(~'binding [~s ~expr] ~@body)))

; todo support both let and var by resolving var

(defmacro bindx [[s expr & more :as bindings] & body]
  (pyramid 'binding bindings body))

(tests
  (macroexpand-1 '(bindx [a 1 b (inc a)] (println a) (+ a b)))
  := '(binding [a 1]
        (binding [b (inc a)]
          (println a)
          (+ a b)))

  (def ^:dynamic a)
  (def ^:dynamic b)
  (bindx [a 1 b (inc a)] (+ a b)) := 3)