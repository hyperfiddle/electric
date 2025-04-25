(ns hyperfiddle.incseq.from-stateful-impl-test
  (:require [hyperfiddle.incseq.stateful-diff-impl :as d]
            [hyperfiddle.incseq.perm-impl :as p]
            [hyperfiddle.incseq.diff-impl :as s]
            [hyperfiddle.incseq.from-stateful-impl :as fs]
            [clojure.test :as t])
  (:import #?(:clj [clojure.lang IFn IDeref])
           [missionary Cancelled]))

(t/deftest basic
  (let [q #?(:clj (let [q (java.util.LinkedList.)]
                    (fn
                      ([] (.remove q))
                      ([x] (.add q x) nil)))
             :cljs (let [q (make-array 0)]
                     (fn
                       ([]
                        (when (zero? (alength q))
                          (throw (js/Error. "No such element.")))
                        (.shift q))
                       ([x] (.push q x) nil))))
        ps ((fs/flow (fn [step done]
                       (q [step done])
                       (step)
                       (reify
                         IFn
                         (#?(:clj invoke :cljs -invoke) [_]
                           (q :cancel))
                         IDeref
                         (#?(:clj deref :cljs -deref) [_]
                           (q)))))
            #(q :step) #(q :done))
        [step done] (q)
        _ (t/is (= (q) :step))
        _ (q (assoc (d/empty-diff 2)
               :change {0 :foo 1 :bar}
               :grow 2))
        diff @ps
        _ (t/is (= ((juxt (comp count :append) :degree :shrink :permutation) diff)
                  [2 2 0 {}]))
        [item0 item1] (:append diff)
        ps0 (item0 #(q :step0) #(q :done0))
        _ (t/is (= (q) :step0))
        _ (t/is (= @ps0 :foo))
        ps1 (item1 #(q :step1) #(q :done1))
        _ (t/is (= (q) :step1))
        _ (step)
        _ (t/is (= (hash-set (q) (q)) #{:step :step0}))
        _ (q (assoc (d/empty-diff 2)
               :permutation (p/transposition 0 1)
               :change {1 :foo 0 :BAR}))
        _ (t/is (= @ps (s/diff [] 2 0 (p/transposition 0 1))))
        _ (t/is (= @ps1 :BAR))
        _ (t/is (= @ps0 :foo))
        _ (ps0)
        _ (t/is (= (q) :step0))
        ps0- (item0 #(q :step0-) #(q :done0-))
        _ (t/is (= (q) :step0-))
        _ (t/is (= nil (try @ps0 (catch Cancelled _))))
        _ (t/is (= (q) :done0))
        _ (step)
        _ (t/is (= (hash-set (q) (q)) #{:step :step1}))
        _ (q (assoc (d/empty-diff 2)
               :change {1 :FOO}))
        _ (t/is (= @ps0- :FOO))
        _ (t/is (= nil (try (item1 #(q :step1-) #(q :done1-))
                                (catch #?(:clj Error :cljs js/Error) _))))
        _ (step)
        _ (t/is (= (hash-set (q)) #{:step0-}))
        _ (q (assoc (d/empty-diff 2)
               :freeze #{0 1}))
        _ (t/is (= @ps1 :BAR))
        _ (t/is (= (q) :done1))
        _ (t/is (= @ps0- :FOO))
        _ (t/is (= (q) :done0-))
        _ (t/is (= @ps (s/empty-diff 2)))
        _ (done)
        _ (t/is (= (q) :done))]))
