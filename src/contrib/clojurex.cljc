(ns contrib.clojurex
  (:require [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [contrib.clojurex :refer [bindx]])))

(defn binding-pyramid* [bindings body]
  (let [[s expr & xs] bindings]
    (if (seq xs)
      ; don't qualify - for Photon CLJS compatibility ?
      `(~'binding [~s ~expr] ~(binding-pyramid* xs body))
      `(~'binding [~s ~expr] ~@body))))

; todo support both let and var by resolving var

(defmacro bindx [bindings & body] (binding-pyramid* bindings body))

(tests
  (macroexpand-1 '(bindx [a 1 b (inc a)] (inc b)))
  := '(binding [a 1]
        (binding [b (inc a)]
          (inc b)))

  (def ^:dynamic a)
  (def ^:dynamic b)
  (bindx [a 1 b (inc a)] (inc b)) := 3)