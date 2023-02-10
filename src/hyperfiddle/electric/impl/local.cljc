(ns hyperfiddle.electric.impl.local
  #?(:cljs (:require-macros [hyperfiddle.electric.impl.local :refer [local get-local set-local with-local]])))

(defmacro local []
  (if (:js-globals &env) `(volatile! nil) `(ThreadLocal.)))

(defmacro get-local [l]
  (if (:js-globals &env) `(deref ~l) `(.get ~(with-meta l (assoc (meta l) :tag `ThreadLocal)))))

(defmacro set-local [l x]
  (if (:js-globals &env) `(vreset! ~l ~x) `(doto ~x (->> (.set ~(with-meta l (assoc (meta l) :tag `ThreadLocal)))))))

(defmacro with-local [l i & body]
  `(let [prev# (get-local ~l)]
     (set-local ~l ~i)
     (try [(do ~@body) (get-local ~l)]
          (finally (set-local ~l prev#)))))
