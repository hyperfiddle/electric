(ns hyperfiddle.incseq.arrays-impl
  (:refer-clojure :exclude [int-array]))

(defn int-array ^ints [n]
  #?(:clj (make-array Integer/TYPE n)
     :cljs (let [a (make-array n)]
             (dotimes [i n] (aset a i 0)) a)))

(defn acopy [source source-offset target target-offset length]
  #?(:clj (System/arraycopy source source-offset target target-offset length)
     :cljs (dotimes [i length]
             (aset target (+ target-offset i)
               (aget source (+ source-offset i))))))

(defn aget-aset [^objects arr i x]
  (let [y (aget arr i)]
    (aset arr i x) y))

(defn weight-tree [size]
  (let [o (loop [o 1]
            (if (< o size)
              (recur (bit-shift-left o 1)) o))
        n (bit-shift-left o 1)
        arr (int-array n)]
    (loop [f (unchecked-subtract o size)
           o o
           n n]
      (when (< 1 o)
        (loop [i (unchecked-subtract n f)]
          (when (< i n)
            (aset arr i 1)
            (recur (unchecked-inc i))))
        (recur (bit-shift-right f 1)
          (bit-shift-right o 1) o))) arr))