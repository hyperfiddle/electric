(ns contrib.dynamic
  (:require [hyperfiddle.rcf :refer [tests]]))

#?(:clj (def resolve #_(memoize) (comp deref requiring-resolve)))

(defn call-sym [qualified-sym & args]
  #?(:clj (apply (resolve qualified-sym) args)
     :cljs (assert false "contrib.dynamic/call-sym unimplemented in cljs")))

#?(:clj (tests (call-sym 'contrib.str/empty->nil "") := nil))