(ns hfdl.lib
  (:require [hfdl.lang :refer [spawn <| |> dataflow]]))

(defmacro $ [f & args]
  `(spawn (~f ~@(map (partial list `|>) args))))

(defmacro ifn [args & body]
  `(fn ~args (dataflow (let [~@(mapcat (juxt identity (partial list `<|)) args)] ~@body))))