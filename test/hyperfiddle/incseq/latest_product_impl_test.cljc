(ns hyperfiddle.incseq.latest-product-impl-test
  (:require [hyperfiddle.incseq.latest-product-impl :refer [flow]]
            [clojure.test :refer [deftest is testing]])
  #?(:clj (:import (clojure.lang IFn IDeref))))

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

(deftest large-input
  (let [d1 {:grow 32 :degree 32 :shrink 0
            :permutation {}
            :change (zipmap (range 32) (range 32))
            :freeze #{}}
        d2 {:grow 1 :degree 33 :shrink 0
            :permutation {}
            :change {32 32}
            :freeze #{}}
        q (queue)
        ps ((flow identity
              (fn [step done]
                (q step)
                (step)
                (->Ps #(q :cancel) q)))
            #(q :step) #(q :done))
        step (q)]
    (is (= (q) :step))
    (q d1)
    (is (= @ps d1))
    (step)
    (is (= (q) :step))
    (q d2)
    (is (= @ps d2))))