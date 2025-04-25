(ns hyperfiddle.incseq.diff-impl-test
  (:require [clojure.test :refer [deftest is]]
            [hyperfiddle.incseq.diff-impl :as s]
            [hyperfiddle.incseq.perm-impl :as p]))

(deftest combine-simple
  (is (= (s/combine
           (s/diff [:a] 1 0 {})
           (s/diff [] 1 1 {}))
        (s/diff [] 0 0 {})))
  (is (= (s/combine
           (s/diff [:e] 4 2 (p/rotation 1 3))
           (s/diff [:f :g] 4 1 (p/rotation 3 1)))
        (s/diff [:f :g] 5 2 (p/split-swap 1 2 2)))))