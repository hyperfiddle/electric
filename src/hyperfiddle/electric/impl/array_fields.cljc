(ns hyperfiddle.electric.impl.array-fields
  (:refer-clojure :exclude [get set])
  #?(:cljs (:require-macros hyperfiddle.electric.impl.array-fields))
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]))
;; #?(:clj (set! *warn-on-reflection* true))
(defmacro deffields [& fields]
  `(do ~@(for [[fld idx] (mapv vector fields (range))]
           `(def ~fld (int ~idx)))))
(defn swap
  ([^objects a k f] (aset a k (f (aget a k))))
  ([^objects a k f x] (aset a k (f (aget a k) x)))
  ([^objects a k f x y] (aset a k (f (aget a k) x y)))
  ([^objects a k f x y z] (aset a k (f (aget a k) x y z)))
  ([^objects a k f x y z & more] (aset a k (apply f (aget a k) x y z more))))
(defmacro fswap [O k f & args] `(swap (.-state- ~O) ~k ~f ~@args))
(defn get [^objects a k] (aget a k))
(defmacro fget [O k] `(get (.-state- ~O) ~k))
(defmacro set [arr & kvs]
  (let [ar (with-meta (gensym "arr") {:tag 'objects})]
    `(let [~ar ~arr]
       ~@(for [[k v] (partition 2 kvs)]
           ;; FIXME better way to fix reflection warning than call `identity`?
           `(aset ~ar ~k (identity ~v))))))
(defmacro fset [O & kvs] `(set (.-state- ~O) ~@kvs))
(defn getset [^objects a k v] (let [ret (aget a k)] (aset a k v) ret))
(defmacro fgetset [O k v] `(getset (.-state- ~O) ~k ~v))
(defn getswap [^objects a k f] (let [ret (aget a k)] (swap a k f) ret))

;;; TESTS ;;;
(deftype P [state-])
(tests
  (deffields x y)
  (def aP (->P (object-array 2)))
  (let [^P aP aP]
    (fset aP x 1 y 2)            := 2
    [(fget aP x) (fget aP y)]    := [1 2]
    (fswap aP x inc)             := 2
    (swap (.-state- aP) x inc)   := 3
    (fgetset aP x 0)             := 3
    (getset (.-state- aP) x 100) := 0
    (fget aP x)                  := 100
    (getswap (.-state- aP) x inc) := 100
    (fget aP x)                   := 101
    ))
