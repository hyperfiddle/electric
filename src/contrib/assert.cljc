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

(defn -is [v pred vq predq msg ex-data]
  (when-not (pred v)
    ;; throws don't show up in electric yet
    (throw (ex-info (str "assertion failed: (" (pr-str predq) " " (pr-str vq) ") for " (pr-str vq) " = " (pr-str v)
                      (when msg (str "\n\n    " msg)))
             (assoc ex-data ::v v ::pred pred)))
    #_(#?(:clj println :cljs js/console.error)
      (str "assertion failed: (" (pr-str predq) " " (pr-str vq) ") for " (pr-str vq) " = " (pr-str v)
        (when msg (str "\n\n    " msg)) (when (seq ex-data) (str "\n\n" ex-data)))))
  v)

(defmacro is
  ([v] `(is ~v some?))
  ([v pred] `(is ~v ~pred nil))
  ([v pred msg] `(is ~v ~pred ~msg {}))
  ([v pred msg ex-data] `(-is ~v ~pred '~v '~pred ~msg ~ex-data)))

(tests
  (check nil) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check odd? 2) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check odd? 1) := 1
  (check ::done ::done) := ::done
  (check ::done ::not-done) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check ::done 42) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check ::done nil) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error))
