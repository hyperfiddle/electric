(ns hyperfiddle.incseq.diff-impl-test
  (:require [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p]
            [clojure.test :refer [deftest is]]))

(deftest combine-simple
  (is (= (d/combine
           {:grow 1
            :degree 1
            :shrink 0
            :permutation {}
            :change {0 :a}
            :freeze #{}}
           {:grow 0
            :degree 1
            :shrink 1
            :permutation {}
            :change {}
            :freeze #{}})
        {:grow 0
         :degree 0
         :shrink 0
         :permutation {}
         :change {}
         :freeze #{}}))
  (is (= (d/combine
           {:grow 1
            :degree 4
            :shrink 2
            :permutation (p/rotation 3 1)
            :change {1 :e}
            :freeze #{}}
           {:grow 2
            :degree 4
            :shrink 1
            :permutation (p/rotation 1 3)
            :change {0 :f 1 :g 2 :h}
            :freeze #{}})
        {:grow 2
         :degree 5
         :shrink 2
         :permutation (p/compose (p/cycle 2 4) (p/cycle 1 3))
         :change {0 :f, 1 :g, 2 :h}
         :freeze #{}})))

(deftest combine-grow-dont-move
  (is (= (d/combine
           {:grow        2
            :degree      2
            :shrink      0
            :permutation {}
            :change      {0 :x1, 1 :y1}
            :freeze      #{}}
           {:grow        3
            :shrink      0
            :degree      5
            :permutation {0 2, 1 3, 2 4, 3 0, 4 1}
            :change      {0 :x0, 1 :y0, 2 :z0}
            :freeze      #{}})
        {:grow   5
         :degree 5
         :shrink 0
         :permutation {}
         :change {3 :x1, 4 :y1, 0 :x0, 1 :y0, 2 :z0}
         :freeze #{}})))

(deftest grow-permutation-simplifies
  (is (= (d/combine ;; [:z] -> [:z] -> [:x :y :z]
           {:degree 1, :permutation {}, :grow 0, :shrink 0, :change {}, :freeze #{}} ; [:z]
           {:grow 2, :shrink 0, :degree 3, :permutation {0 1, 1 2, 2 0}, :change {0 :x, 1 :y}, :freeze #{}})
        {:degree 3, :permutation {2 0, 0 2}, :grow 2, :shrink 0, :change {0 :x, 1 :y}, :freeze #{}})))

(deftest shrink-permutation-simplifies
  (is (= (d/combine  ;; [:w :x :y :z] -> [:w :x :y :z] -> [:z]
           {:degree 4, :grow 0, :shrink 0, :permutation {},                   :change {}, :freeze #{}}
           {:degree 4, :grow 0, :shrink 3, :permutation {1 0, 2 1, 3 2, 0 3}, :change {}, :freeze #{}})
        {:degree 4, :permutation {0 3, 3 0}, :grow 0, :shrink 3, :change {}, :freeze #{}})))
