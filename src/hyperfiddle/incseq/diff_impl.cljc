(ns hyperfiddle.incseq.diff-impl
  (:require [hyperfiddle.incseq.perm-impl :as p]))

(defrecord Diff [append degree shrink permutation])

(def diff ->Diff)
(defn append [^Diff d] (.-append d))
(defn degree [^Diff d] (.-degree d))
(defn shrink [^Diff d] (.-shrink d))
(defn permutation [^Diff d] (.-permutation d))

(defn empty-diff [degree]
  (diff [] degree 0 {}))

(defn empty-diff? [d]
  (-> (shrink d)
    (unchecked-add (count (append d)))
    (unchecked-add (count (permutation d)))
    (zero?)))

(defn combine [x y]
  (let [grow-x (count (append x))
        grow-y (count (append y))
        max-degree   (unchecked-add      (degree y) (shrink x))
        size-before  (unchecked-subtract (degree x) grow-x)
        size-between (unchecked-subtract (degree y) grow-y)
        size-after   (unchecked-subtract (degree y) (shrink y))
        permutation  (p/compose
                       (permutation x)
                       (p/split-swap size-between grow-y (shrink x))
                       (permutation y))]
    (loop [a (transient [])
           d max-degree
           p permutation
           i 0
           j size-before]
      (if (< i grow-x)
        (let [k (p j j)]
          (if (< k size-after)
            (recur (conj! a (nth (append x) i)) d p
              (unchecked-inc i) (unchecked-inc j))
            (recur a (unchecked-dec d)
              (p/compose (p/rotation j d)
                p (p/rotation d k))
              (unchecked-inc i) j)))
        (diff (persistent! (reduce conj! a (append y))) d
          (unchecked-subtract d size-after) p)))))