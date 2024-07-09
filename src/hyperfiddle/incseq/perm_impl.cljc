(ns hyperfiddle.incseq.perm-impl
  (:refer-clojure :exclude [cycle])
  (:require [hyperfiddle.rcf :refer [tests]]
            [clojure.set]))

(def inverse clojure.set/map-invert)

(defn cycle
  ([_] {})
  ([i & js] (zipmap `(~i ~@js) `(~@js ~i))))

(defn rotation [i j]
  (case (compare i j)
    -1 (apply cycle (range i (inc j) +1))
    0 {}
    +1 (apply cycle (range i (dec j) -1))))

(defn split-swap [i l r]
  (let [l (int l)
        r (int r)]
    (case l
      0 {}
      (case r
        0 {}
        (let [j (unchecked-add-int i l)
              k (unchecked-add-int j r)]
          (zipmap (range i k)
            (concat (range j k)
              (range i j))))))))

(defn arrange [v p]
  (persistent!
    (reduce-kv
      (fn [r i j]
        (assoc! r i (nth v j)))
      (transient v) p)))

(defn decompose [rf r p]
  (loop [p p
         r r]
    (case p
      {} r
      (let [[i j] (first p)]
        (let [c (loop [c [i]
                       j j]
                  (let [c (conj c j)
                        j (p j)]
                    (if (== i j)
                      c (recur c j))))
              r (rf r c)]
          (if (reduced? r)
            @r (recur (apply dissoc p c) r)))))))

(defn compose
  ([] {})
  ([x] x)
  ([x y]
   (reduce-kv
     (fn [r i j]
       (let [k (y j j)]
         (if (== i k)
           (dissoc r i)
           (assoc r i k))))
     y x))
  ([x y & zs]
   (reduce compose (compose x y) zs)))

(defn order [p]
  (loop [o 1, q p]
    (case q
      {} o
      (recur (unchecked-inc o)
        (compose p q)))))

(defn involution? [p]
  (and (not= {} p) (= {} (compose p p))))

(defn transposition? [p]
  (= 2 (count p)))

(defn recompose [cycles]
  (->> cycles
    (eduction (map (partial apply cycle)))
    (reduce compose (compose))))

(defn split-long-swap [o l c r]
  (->> (range o (+ o (min l r)))
    (eduction (map (fn [i] (cycle i (+ l c i)))))
    (reduce compose {})
    (compose
      (case (compare l r)
        -1 (split-swap (+ o l) (+ l c) (- r l))
        0 {}
        +1 (split-swap (+ o r) (- l r) (+ c r))))))

(defn cycle-transpositions [rf r cycle]
  (loop [r r
         i (unchecked-dec-int (count cycle))]
    (if (zero? i)
      r (let [x (cycle 0)
              y (cycle i)
              r (if (< x y)
                  (rf r x y)
                  (rf r y x))]
          (if (reduced? r)
            @r (recur r (unchecked-dec-int i)))))))

(defn transpositions [rf r p]
  (decompose (partial cycle-transpositions rf) r p))

(defn transposition-rotations [rf r i j]
  (let [k (dec j)
        r (rf r i j)]
    (if (reduced? r)
      @r (if (== k i)
           r (unreduced
               (rf r k i))))))

;; TODO generate optimal sequence if possible
(defn rotations [rf r p]
  (decompose
    (->> rf
      (partial transposition-rotations)
      (partial cycle-transpositions))
    r p))