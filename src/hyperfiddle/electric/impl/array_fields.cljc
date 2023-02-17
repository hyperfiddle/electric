(ns hyperfiddle.electric.impl.array-fields
  (:refer-clojure :exclude [get set])
  #?(:cljs (:require-macros hyperfiddle.electric.impl.array-fields))
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]))
;; #?(:clj (set! *warn-on-reflection* true))
(defmacro deffields [& fields]
  `(do ~@(for [[fld idx] (mapv vector fields (range))]
           `(def ~fld (int ~idx)))))
(defmacro swap [arr k f & args]
  (let [ar (with-meta (gensym "arr") {:tag 'objects})]
    `(let [~ar ~arr, k# ~k, f# ~f]
       (aset ~ar k# (f# (aget ~ar k#) ~@args)))))
(defmacro fswap [O k f & args] `(swap (.-state- ~O) ~k ~f ~@args))
(defmacro get [arr k]
  (let [ar (with-meta (gensym "arr") {:tag 'objects})]
    `(let [~ar ~arr] (aget ~ar ~k))))
(defmacro fget [O k] `(get (.-state- ~O) ~k))
(defmacro set [arr & kvs]
  (let [ar (with-meta (gensym "arr") {:tag 'objects})]
    `(let [~ar ~arr]
       ~@(for [[k v] (partition 2 kvs)]
           ;; FIXME better way to fix reflection warning than call `identity`?
           `(aset ~ar ~k (identity ~v))))))
(defmacro fset [O & kvs] `(set (.-state- ~O) ~@kvs))
(defmacro fgetset [O k v] `(getset (.-state- ~O) ~k ~v))
(defmacro getset [arr k v]
  (let [ar (with-meta (gensym "arr") {:tag 'objects})]
    `(let [~ar ~arr, k# ~k, ret# (get ~ar k#)]
       (set ~ar k# ~v)
       ret#)))
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
    ))
