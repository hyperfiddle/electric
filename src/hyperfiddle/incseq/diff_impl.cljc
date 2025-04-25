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

;; workaround - peek doesn't work on transient vectors :/
(defn tpeek [tv]
  (nth tv (dec (count tv))))

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
                       (permutation y))
        a+p (reduce-kv (fn [a+p i x]
                         (let [p (tpeek a+p)
                               a (pop! a+p)
                               j (unchecked-add size-before i)
                               k (permutation j j)]
                           (if (< k size-after)
                             (-> a (conj! x) (conj! p))
                             (conj! a (p/compose (p/rotation j max-degree)
                                        p (p/rotation max-degree k))))))
              (conj! (transient []) permutation) (append x))
        permutation (tpeek a+p)
        appendx (pop! a+p)
        elided (unchecked-subtract grow-x (count appendx))
        append (persistent! (reduce conj! appendx (append y)))
        degree (unchecked-subtract max-degree elided)
        shrink (unchecked-subtract degree size-after)]
    ;; TODO remove shrunk reorders (still needed ?)
    (diff append degree shrink permutation)))