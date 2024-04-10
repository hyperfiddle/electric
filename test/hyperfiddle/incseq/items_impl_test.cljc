(ns hyperfiddle.incseq.items-impl-test
  (:require [hyperfiddle.incseq.diff-impl :as d]
            [hyperfiddle.incseq.items-impl :as ii]
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
        ps ((ii/flow (fn [step done]
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
        _ (t/is (= (dissoc diff :change)
                (assoc (dissoc (d/empty-diff 2) :change)
                  :freeze #{0 1}
                  :grow 2)))
        [item0 item1] (map (:change diff) [0 1])
        ps0 (item0 #(q :step0) #(q :done0))
        _ (t/is (= (q) :step0))
        _ (t/is (= @ps0 :foo))
        ps1 (item1 #(q :step1) #(q :done1))
        _ (t/is (= (q) :step1))
        _ (step)
        _ (t/is (= (hash-set (q) (q)) #{:step :step0}))
        _ (q (assoc (d/empty-diff 2)
               :permutation {0 1 1 0}
               :change {1 :foo 0 :BAR}))
        _ (t/is (= @ps (assoc (d/empty-diff 2) :permutation {0 1 1 0})))
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
        _ (t/is (= @ps (d/empty-diff 2)))
        _ (done)
        _ (t/is (= (q) :done))]))
