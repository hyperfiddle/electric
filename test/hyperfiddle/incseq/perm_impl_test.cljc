(ns hyperfiddle.incseq.perm-impl-test
  (:require [hyperfiddle.incseq.perm-impl :as p]
            [clojure.test :refer [deftest is]]))

(deftest suite
  (is (= (p/decompose conj #{} {0 1, 1 4, 2 3, 3 2, 4 0})
        #{[0 1 4] [2 3]}))

  (is (= (p/recompose #{[0 1 4] [2 3]})
        {0 1, 1 4, 2 3, 3 2, 4 0}))

  (is (= (p/decompose conj #{} (p/inverse {0 1, 1 4, 2 3, 3 2, 4 0}))
        #{[1 0 4] [3 2]}))

  (is (= (p/recompose #{[1 0 4] [3 2]})
        {0 4, 1 0, 2 3, 3 2, 4 1}))

  (is (= (p/arrange [0 1 2 3 4] {0 1, 1 4, 2 3, 3 2, 4 0})
        [1 4 3 2 0]))

  (is (= (p/arrange [:a :b :c :d :e] {0 1, 1 4, 2 3, 3 2, 4 0})
        [:b :e :d :c :a]))

  (is (= (p/compose
           (p/cycle [1 3 2 4])
           (p/cycle [1 4 2 3]))
        {}))

  (is (= (p/inverse (p/split-swap 4 2 3))
        (p/split-swap 4 3 2)))

  (is (= (p/order {}) 1))

  (is (= (p/order (p/cycle [2 3])) 2))

  (is (= (p/order (p/cycle [2 3 4])) 3))

  (is (= (p/order (p/compose (p/cycle [0 1]) (p/cycle [2 3 4]))) 6))

  (is (= (p/involution? {}) false))

  (is (= (p/involution? (p/cycle [2 3])) true))

  (is (= (p/involution? (p/cycle [2 3 4])) false))

  (is (= (p/transposition? (p/cycle [2 3])) true))

  (is (= (p/transposition? (p/cycle [2 3 4])) false))

  (is (= (p/rotations (fn [r i j] (p/compose (p/rotation i j) r))
           {} {0 1, 1 4, 2 3, 3 2, 4 0})
        {0 1, 1 4, 2 3, 3 2, 4 0})))