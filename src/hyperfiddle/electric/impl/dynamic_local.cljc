(ns hyperfiddle.electric.impl.dynamic-local
  (:refer-clojure :exclude [get set])
  #?(:cljs (:require-macros hyperfiddle.electric.impl.dynamic-local)))

(defn local []
  #?(:clj (new ThreadLocal)
     :cljs (object-array 1)))

(defn set [local v]
  #?(:clj (.set ^ThreadLocal local v)
     :cljs (aset local 0 v)))

(defn get [local]
  #?(:clj (.get ^ThreadLocal local)
     :cljs (aget local 0)))

(defmacro bind [[local v] & body]
  `(let [local# ~local, prev# (get local#)]
     (try (set local# ~v) (do ~@body)
          (finally (set local# prev#)))))
