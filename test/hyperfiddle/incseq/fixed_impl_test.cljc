(ns hyperfiddle.incseq.fixed-impl-test
  (:require [hyperfiddle.incseq.fixed-impl :refer [flow]]
            [clojure.test :refer [deftest is]]))

(defn queue []
  #?(:clj (let [q (java.util.LinkedList.)]
            (fn
              ([] (.remove q))
              ([x] (.add q x) nil)))
     :cljs (let [q (make-array 0)]
             (fn
               ([]
                (when (zero? (alength q))
                  (throw (js/Error. "No such element.")))
                (.shift q))
               ([x] (.push q x) nil)))))

(deftype Ps [cancel transfer]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (cancel))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (transfer)))

(deftest zero
  (let [q (queue)
        ps ((flow) #(q :step) #(q :done))]
    (is (= (q) :step))
    @ps := {:grow 0
            :degree 0
            :shrink 0
            :permutation {}
            :change {}
            :freeze #{}}))

(deftest one
  (let [q (queue)
        ps ((flow (fn [n t] (q n) (n) (->Ps #(q :cancel) q)))
            #(q :step) #(q :done))
        n (q)]
    (q) := :step
    (q 0)
    @ps := {:grow 1, :degree 1, :shrink 0, :permutation {}, :change {0 0}, :freeze #{}}
    (n)
    (q) := :step
    (q 1)
    @ps := {:grow 0, :shrink 0, :degree 1, :permutation {}, :change {0 1}, :freeze #{}}))

(deftest two
  (let [q (queue)
        ps ((flow
              (fn [n t] (q n) (->Ps #(q :cancel) q))
              (fn [n t] (q n) (->Ps #(q :cancel) q)))
            #(q :step) #(q :done))
        n1 (q)
        n2 (q)]
    (n1)
    (q) := :step
    (n2)
    (q 0)
    (q :a)
    @ps := {:grow 2
            :degree 2
            :shrink 0
            :permutation {}
            :change {0 0, 1 :a}
            :freeze #{}}
    (n1)
    (q) := :step
    (n2)
    (q 1)
    (q :b)
    @ps := {:grow 0
            :degree 2
            :shrink 0
            :permutation {}
            :change {0 1, 1 :b}
            :freeze #{}}))