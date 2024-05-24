(ns hyperfiddle.incseq.perm-impl
  (:refer-clojure :exclude [cycle])
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn inverse [p] (into {} (map (juxt val key)) p))

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

(defn decompose [p]
  (loop [p p
         cs #{}]
    (case p
      {} cs
      (let [[i j] (first p)]
        (let [c (loop [c [i]
                       j j]
                  (let [c (conj c j)
                        j (p j)]
                    (if (== i j)
                      c (recur c j))))]
          (recur (apply dissoc p c)
            (conj cs c)))))))

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

(tests "permutations"
  (decompose {0 1, 1 4, 2 3, 3 2, 4 0}) :=
  #{[0 1 4] [2 3]}

  (recompose #{[0 1 4] [2 3]}) :=
  {0 1, 1 4, 2 3, 3 2, 4 0}

  (decompose (inverse {0 1, 1 4, 2 3, 3 2, 4 0})) :=
  #{[1 0 4] [3 2]}

  (recompose #{[1 0 4] [3 2]}) :=
  {0 4, 1 0, 2 3, 3 2, 4 1}

  (arrange [0 1 2 3 4] {0 1, 1 4, 2 3, 3 2, 4 0}) :=
  [1 4 3 2 0]

  (arrange [:a :b :c :d :e] {0 1, 1 4, 2 3, 3 2, 4 0}) :=
  [:b :e :d :c :a]

  (compose
    (cycle 1 3 2 4)
    (cycle 1 4 2 3)) := {}

  (inverse (split-swap 4 2 3)) := (split-swap 4 3 2)

  (order (cycle 2)) := 1
  (order (cycle 2 3)) := 2
  (order (cycle 2 3 4)) := 3
  (order (compose (cycle 0 1) (cycle 2 3 4))) := 6

  (involution? (cycle 2)) := false
  (involution? (cycle 2 3)) := true
  (involution? (cycle 2 3 4)) := false

  (transposition? (cycle 2 3)) := true
  (transposition? (cycle 2 3 4)) := false)