(ns hyperfiddle.incseq.mount-impl-test
  (:require [hyperfiddle.domlike :as d]
            [hyperfiddle.incseq.mount-impl :refer [mount]]
            [clojure.test :refer [deftest is]]))

(def mount-items (mount d/append-child d/replace-child d/insert-before d/remove-child d/nth-child))

(deftest grow-shrink-same
  (let [p (d/node)
        a (d/node)
        b (d/node)]
    (d/append-child p a)
    (d/append-child p b)
    (is (= (d/tree p)
          (d/tree (mount-items p
                    {:grow 2
                     :degree 4
                     :shrink 2
                     :permutation {0 2, 1 3, 2 0, 3 1}
                     :change {0 a, 1 b}
                     :freeze #{}}))))))

(deftest append-unordered
  (let [p (d/node)
        a (d/node)
        b (d/node)]
    (= [p [a] [b]]
      (d/tree
        (mount-items p
          {:grow        2
           :degree      2
           :shrink      0
           :permutation {0 1, 1 0}
           :change      {0 a, 1 b}
           :freeze      #{}})))))