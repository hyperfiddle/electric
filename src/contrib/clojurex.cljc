(ns contrib.clojurex
  ;(:refer-clojure :exclude [binding])
  (:require [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [contrib.clojurex :refer [binding-pyramid]])))

(defn binding-pyramid* [bindings body]
  (let [[s expr & xs] bindings]
    (if (seq xs)
      `(clojure.core/binding [~s ~expr] ~(binding-pyramid* xs body))
      `(clojure.core/binding [~s ~expr] ~@body))))

(defmacro binding-pyramid [bindings & body] (binding-pyramid* bindings body))

(tests
  (macroexpand-1 '(binding-pyramid [a 1 b (inc a)] (inc b)))
  := '(clojure.core/binding [a 1]
        (clojure.core/binding [b (inc a)]
          (inc b)))

  (def ^:dynamic a)
  (def ^:dynamic b)
  (binding-pyramid [a 1 b (inc a)] (inc b)) := 3)