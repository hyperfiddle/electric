(ns hyperfiddle.electric.impl.array-fields
  (:refer-clojure :exclude [get set])
  #?(:cljs (:require-macros hyperfiddle.electric.impl.array-fields))
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]))
#?(:clj (set! *warn-on-reflection* true))
(defmacro deffields [& fields]
  `(do ~@(for [[fld idx] (mapv vector fields (range))]
           `(def ~fld (int ~idx)))
       ~(count fields)))
(defn get [^objects a k] (aget a (int k)))
(defn set
  ([^objects a i v] (aset a (int i) v))
  ([^objects a i v i2 v2] (aset a (int i) v) (aset a (int i2) v2))
  ([^objects a i v i2 v2 i3 v3] (aset a (int i) v) (aset a (int i2) v2) (aset a (int i3) v3))
  ([^objects a i v i2 v2 i3 v3 i4 v4] (aset a (int i) v) (aset a (int i2) v2) (aset a (int i3) v3) (aset a (int i4) v4))
  ([^objects a i v i2 v2 i3 v3 i4 v4 & more] (set a i v i2 v2 i3 v3 i4 v4) (apply set a more)))
(defn swap
  ([^objects a k f] (set a k (f (get a k))))
  ([^objects a k f x] (set a k (f (get a k) x)))
  ([^objects a k f x y] (set a k (f (get a k) x y)))
  ([^objects a k f x y z] (set a k (f (get a k) x y z)))
  ([^objects a k f x y z & more] (set a k (apply f (get a k) x y z more))))
(defmacro fswap [O k f & args] `(swap (.-state- ~O) ~k ~f ~@args))
(defmacro fget [O k] `(get (.-state- ~O) ~k))
(defmacro fset [O & kvs] `(set (.-state- ~O) ~@kvs))
(defn getset [^objects a k v] (let [ret (get a k)] (when (not= ret v) (set a k v)) ret))
(defmacro fgetset [O k v] `(getset (.-state- ~O) ~k ~v))
(defn getswap [^objects a k f] (let [ret (get a k)] (swap a k f) ret))
(defn cas [^objects a i oldv newv] (if (= oldv (get a i)) (do (set a i newv) true) false))
(defn ncas [^objects a i oldv newv] (if (not= oldv (get a i)) (do (set a i newv) true) false))

(defn rot
  ([^objects a i j] (let [tmp (get a i)] (set a i (get a j) j tmp)))
  ([^objects a i j k] (let [tmp (get a i)] (set a i (get a j) j (get a k) k tmp)))
  ([^objects a i j k l] (let [tmp (get a i)] (set a i (get a j) j (get a k) k (get a l) l tmp)))
  ([^objects a i j k l & more]
   (let [tmp (get a i)]
     (rot a i j k l)
     (loop [[i j :as more] (seq (cons l more))]
       (if j
         (do (set a i (get a j)) (recur (next more)))
         (set a i tmp))))))


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

(tests
  (let [a (object-array [:a :b])]
    (rot a 0 1)
    (vec a) := [:b :a])
  (let [a (object-array [:a :b :c])]
    (rot a 0 2 1)
    (vec a) := [:c :a :b])
  (let [a (object-array [:a :b :c :d])]
    (rot a 0 2 1 3)
    (vec a) := [:c :d :b :a])
  (let [a (object-array [:a :b :c :d :e :f :g])]
    (apply rot a (range 7))
    (vec a) := [:b :c :d :e :f :g :a]))
