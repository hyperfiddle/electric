(ns hfdl.lib
  (:require [hfdl.lang :refer [spawn <| |> dataflow]]
            [missionary.core :as m]))

(defmacro $ [f & args]
  `(spawn (~f ~@(map (partial list `|>) args))))

(defmacro ifn [args & body]
  `(fn ~args (dataflow (let [~@(mapcat (juxt identity (partial list `<|)) args)] ~@body))))

(defn place! "
Defines a new identity representing a variable initialized with given value and usable both :
* as a continuous flow producing successive states of the variable.
* as a one-argument function assigning a new state to the variable."
  [init]
  (let [!a (atom init)
        >a (m/watch !a)]
    (fn
      ([x] (reset! !a x))
      ([n t] (>a n t)))))