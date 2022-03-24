(ns geoffrey.sorted-set
  (:require [minitest :refer [tests]]))

(defn reverse-index
  "Builds a map of {xᵢ, i} from x[0..n] in `xs`"
  [xs]
  (into {} (map vector xs (range))))

(tests
 (reverse-index nil) := {}
 (reverse-index [:a :b :c]) := {:a 0, :b 1, :c 2})


(defn sort-by-rindex [rindex]
  (comparator #(< (rindex %1) (rindex %2))))

(defn ->sorted-set [xs]
  (into (sorted-set-by (sort-by-rindex (reverse-index xs))) xs))

(tests
 (->sorted-set (range 10)) := (sorted-set 0 1 2 3 4 5 6 7 8 9)
 (seq (disj (->sorted-set (range 10)) 0 2 4 6 8)) := '(1 3 5 7 9)
 )

(defn map-ordered
  "Like `clojure.core/map`, but returns a sorted-set sorted by elements position
  in the original collection `xs`."
  [f xs]
  (let [xs' (map f xs)]
    (into (sorted-set-by (sort-by-rindex (reverse-index xs'))) xs')))

(tests
 ;; map-ordered is about mapping sets.
 (def alphabet (map str "abcdefghij"))
 (set alphabet) := #{"d" "f" "e" "j" "a" "i" "b" "g" "h" "c"}
 (map-ordered keyword alphabet) := (set (map keyword alphabet))

 ;; But it preserves order when sequenced
 (seq (map-ordered keyword alphabet)) := (map keyword alphabet)

 ;; Ordering is not accidental (HashSet vs ArraySet)
 (map-ordered inc (into (sorted-set) (range 10))) := (into (sorted-set) (range 1 11))

 ;; Set-based deduplication
 (seq (map-ordered inc [1 1 2 2 3 3])) := '(2 3 4))

