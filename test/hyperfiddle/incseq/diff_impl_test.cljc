(ns hyperfiddle.incseq.diff-impl-test
  (:require [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.incseq :as i]
            [clojure.test :refer [deftest is]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as tc-prop]
            [clojure.test.check.generators :as tc-gen]
            [clojure.test.check.clojure-test :as tct]))

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

(def patch-vecing-differ-result-returns-same-vector
  (tc-prop/for-all [a (tc-gen/fmap vec (tc-gen/set tc-gen/small-integer))
                    b (tc-gen/fmap vec (tc-gen/set tc-gen/small-integer))]
    (let [a (into [] (distinct) a), b (into [] (distinct) b)
          diff-seq (i/->seq-differ identity)]
      (= b (d/patch-vec a (do (diff-seq a) (diff-seq b)))))))

(tct/defspec patch-vecing-differ-result-returns-same-vector-spec 100 patch-vecing-differ-result-returns-same-vector)

(def d-combine
  (tc-prop/for-all [a (tc-gen/fmap vec (tc-gen/set tc-gen/small-integer))
                    b (tc-gen/fmap vec (tc-gen/set tc-gen/small-integer))
                    c (tc-gen/fmap vec (tc-gen/set tc-gen/small-integer))]
    (let [a (into [] (distinct) a), b (into [] (distinct) b), c (into [] (distinct) c)
          diff-seq (i/->seq-differ identity)
          _ (diff-seq a), a->b (diff-seq b), b->c (diff-seq c)]
      (= c (d/patch-vec a (d/combine a->b b->c))))))

(tct/defspec d-combine-spec 100 d-combine)
