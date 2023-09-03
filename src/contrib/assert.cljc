(ns contrib.assert
  #?(:cljs (:require-macros contrib.assert))
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn- error! [pred-expr v-expr v ex-data]
  (let [msg (str "check failed: (" (pr-str pred-expr) " " (pr-str v-expr) ") for " (pr-str v))]
    (throw (ex-info msg ex-data)))) ; todo elide top frames

(defn -check [pred-expr pred v-expr v ex-data]
  (cond
    (keyword? pred) (when-not (or (= pred v) ; special rule - keyword equality
                                (pred v))
                      (error! pred-expr v-expr v ex-data))
    () (when-not (pred v)
         (error! pred-expr v-expr v ex-data)))
  v)

(defmacro check
  ([v] `(check some? ~v))
  ([pred v] `(check ~pred ~v {}))
  ([pred v ex-data] `(-check '~pred ~pred '~v ~v ~ex-data)))

(tests
  (check nil) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check odd? 2) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check odd? 1) := 1
  (check ::done ::done) := ::done
  (check ::done ::not-done) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check ::done 42) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check ::done nil) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error))
