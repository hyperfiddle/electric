(ns hyperfiddle.incseq.perm-impl
  (:refer-clojure :exclude [cycle])
  (:require [clojure.set]))

(def inverse clojure.set/map-invert)

(defn transposition [i j]
  {i j, j i})

(defn cycle
  ([xs]
   (let [n (count xs)
         f (nth xs 0)]
     (loop [j f
            i 1
            p (transient {})]
       (let [k (nth xs i)
             i (unchecked-inc i)
             p (assoc! p j k)]
         (if (< i n)
           (recur k i p)
           (persistent! (assoc! p k f))))))))

(defn rotation [i j]
  (case (compare i j)
    -1 (cycle (range i (inc j) +1))
    0 {}
    +1 (cycle (range i (dec j) -1))))

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
    (if-some [[i j] (first p)]
      (let [c (loop [c (transient [i])
                     j j]
                (let [c (conj! c j)
                      k (p j)]
                  (if (== i k)
                    (persistent! c)
                    (recur c k))))
            r (rf r c)]
        (if (reduced? r)
          @r (recur (persistent! (reduce dissoc! (transient p) c)) r))) r)))

(defn transpositions [rf r p]
  (if-some [[i j] (first p)]
    (loop [p (dissoc! (transient p) i)
           r (rf r i j)
           i i
           j j]
      (if (reduced? r)
        @r (let [k (p j)
                 p (dissoc! p j)]
             (if (== i k)
               (let [p (persistent! p)]
                 (if-some [[i j] (first p)]
                   (recur (dissoc! (transient p) i)
                     (rf r i j) i j) r))
               (recur p (rf r j k) i k))))) r))

(defn reverse-transpositions [rf r p]
  (if-some [[i j] (first p)]
    (loop [p (dissoc! (transient p) i)
           r (rf r i j)
           i i
           j j]
      (if (reduced? r)
        @r (let [k (p j)
                 p (dissoc! p j)]
             (if (== i k)
               (let [p (persistent! p)]
                 (if-some [[i j] (first p)]
                   (recur (dissoc! (transient p) i)
                     (rf r i j) i j) r))
               (recur p (rf r i k) i k))))) r))

(defn assoc-if-absent! [m k v]
  (when (contains? m k) (throw (new #?(:clj Error :cljs js/Error) "compose failure - non-disjoint permutations")))
  (assoc! m k v))

(defn safe-merge [x y]
  (persistent! (reduce-kv assoc-if-absent! (transient x) y)))

(defn compose-disjoint [x y]
  (if (< (count x) (count y))
    (safe-merge y x)
    (safe-merge x y)))

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
    (eduction (map cycle))
    (reduce compose (compose))))

(defn split-long-swap [o l c r]
  (->> (range o (+ o (min l r)))
    (eduction (map (fn [i] (transposition i (+ l c i)))))
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