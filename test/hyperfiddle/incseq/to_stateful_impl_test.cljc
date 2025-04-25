(ns hyperfiddle.incseq.to-stateful-impl-test
  (:require [hyperfiddle.incseq.to-stateful-impl :refer [flow]]
            [hyperfiddle.incseq.diff-impl :as d]
            [clojure.test :refer [deftest is]])
  #?(:clj (:import (clojure.lang IFn IDeref)
                   (java.util LinkedList))))

(defn queue []
  #?(:clj (let [q (LinkedList.)]
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

(deftest basic
  (let [q (queue)
        ps ((flow (fn [step done]
                    (step)
                    (q step)
                    (->Ps #(q :cancel) q)))
            #(q :step) #(q :done))
        step (q)]
    (is (= (q) :step))
    (q (d/empty-diff 0))
    (is (= @ps {:grow 0 :degree 0 :shrink 0 :permutation {} :change {} :freeze #{}}))
    (step)
    (is (= (q) :step))
    (q (d/diff [(fn [step done] (step) (q step) (q done) (->Ps #((q)) #((q))))] 1 0 {}))
    (q (constantly :foo))
    (is (= @ps {:grow 1 :degree 1 :shrink 0 :permutation {} :change {0 :foo} :freeze #{}}))
    (let [step-inner (q)
          done-inner (q)]
      (step-inner)
      (is (= (q) :step))
      (q (constantly :bar))
      (is (= @ps {:grow 0 :degree 1 :shrink 0 :permutation {} :change {0 :bar} :freeze #{}}))
      (step)
      (is (= (q) :step))
      (q (d/diff [] 1 1 {}))
      (q step-inner)
      (q #(do (done-inner) (throw #?(:clj (Error.) :cljs (js/Error.)))))
      (is (= @ps {:grow 0 :degree 1 :shrink 1 :permutation {} :change {} :freeze #{}})))))