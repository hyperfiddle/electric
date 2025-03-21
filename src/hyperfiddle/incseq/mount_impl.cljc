(ns hyperfiddle.incseq.mount-impl
  (:require [hyperfiddle.incseq.perm-impl :as p]))

(defn permute-keys [rf r p m]
  (reduce-kv (fn [r k v] (rf r (p k k) v)) r m))

(defn mount [append-child replace-child insert-before remove-child nth-child]
  (letfn [(append [element degree grow permutation change]
            (let [q (p/inverse permutation)]
              (loop [i (- degree grow)
                     c change
                     p permutation]
                (if (== i degree)
                  (do (permute-keys replace element p c)
                      (p/rotations rotate element p))
                  (let [j (q i i)
                        e (c j)]
                    (recur (inc i) (dissoc c j)
                      (if (< j i)
                        (do (insert-before element e (nth-child element j))
                            (p/compose p (p/rotation j i)))
                        (do (append-child element e) p))))))))
          (replace [element i e]
            (replace-child element e (nth-child element i)))
          (rotate [element i j]
            (insert-before element (nth-child element i)
              (nth-child element (if (< i j) (inc j) j)))
            element)]
    (fn [element {:keys [grow shrink degree permutation change]}]
      (let [size-after (- degree shrink)]
        (loop [d degree
               p permutation]
          (if (== d size-after)
            (append element d grow p change)
            (let [i (dec d)
                  j (p i i)]
              (remove-child element (nth-child element j))
              (recur i (p/compose p (p/rotation i j))))))))))