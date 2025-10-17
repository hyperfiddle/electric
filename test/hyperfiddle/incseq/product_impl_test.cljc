(ns hyperfiddle.incseq.product-impl-test
  (:require [clojure.test :refer [deftest is]]
            [hyperfiddle.incseq.diff-impl :as s]
            [hyperfiddle.incseq.product-impl :refer [flow]])
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

(deftest ident
  (let [d1 (s/diff [:foo :bar] 2 0 {})
        d2 (s/diff [:baz] 3 0 {2 0 0 1 1 2})
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