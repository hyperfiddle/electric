(ns hyperfiddle.incseq.latest-concat-impl-test
  (:require [hyperfiddle.incseq :as i]
            [hyperfiddle.incseq.latest-concat-impl :refer [flow]]
            [clojure.test :refer [deftest is]])
  (:import #?(:clj (clojure.lang IFn IDeref))
           missionary.Cancelled))

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

(defn error [^String msg]
  (new #?(:clj Error :cljs js/Error) msg))

(deftype Ps [cancel transfer]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (cancel))
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (transfer)))

(deftest simple-concat
  (let [q (queue)
        ps ((flow (fn [step done] (step) (->Ps #(q :cancel) q)))
            #(q :step) #(q :done))]
    (is (= (q) :step))
    (q {:grow   2
        :degree 2
        :shrink 0
        :permutation {}
        :freeze #{}
        :change {0 (fn [step done]
                     (step)
                     (->Ps #(q :cancel)
                       #(-> {:grow 3
                             :degree 3
                             :shrink 0
                             :permutation {}
                             :change {0 :x0
                                      1 :y0
                                      2 :z0}
                             :freeze #{}})))
                 1 (fn [step done]
                     (step)
                     (->Ps #(q :cancel)
                       #(-> {:grow 2
                             :degree 2
                             :shrink 0
                             :permutation {}
                             :change {0 :x1
                                      1 :y1}
                             :freeze #{}})))}})
    (is (= @ps {:degree      5
                :grow        5
                :shrink      0
                :permutation {}
                :change      {0 :x0
                              1 :y0
                              2 :z0
                              3 :x1
                              4 :y1}
                :freeze      #{}}))))

(deftest inner-change
  (let [q (queue)
        ps ((flow (fn [step done] (q step) (step) (->Ps #(q :cancel) q)))
            #(q :step) #(q :done))
        step (q)]
    (is (= (q) :step))
    (q {:grow   1
        :degree 1
        :shrink 0
        :permutation {}
        :freeze #{}
        :change {0 (fn [step done] (step) (->Ps #(q :cancel) q))}})
    (q {:grow   1
        :degree 1
        :shrink 0
        :permutation {}
        :change {0 :foo}
        :freeze #{}})
    (is (= @ps {:grow        1
                :degree      1
                :shrink      0
                :permutation {}
                :change      {0 :foo}
                :freeze      #{}}))
    (step)
    (is (= (q) :step))
    (q {:grow   0
        :degree 1
        :shrink 0
        :permutation {}
        :freeze #{}
        :change {0 (fn [step done] (step) (->Ps #(q :cancel) q))}})
    (q {:grow   1
        :degree 1
        :shrink 0
        :permutation {}
        :change {0 :foo}
        :freeze #{}})
    (is (= @ps {:grow        1
                :degree      2
                :shrink      1
                :permutation {0 1, 1 0}
                :change      {0 :foo}
                :freeze      #{}}))
    (is (= (q) :cancel))))

(deftest outer-change
  (let [q (queue)
        ps ((flow (fn [step done] (q step) (step) (->Ps #(q :cancel) q)))
            #(q :step) #(q :done))
        step (q)]
    (is (= (q) :step))
    (q {:grow   2
        :degree 2
        :shrink 0
        :permutation {}
        :freeze #{}
        :change {0 (fn [step done] (step) (->Ps #(q :cancel) q))
                 1 (fn [step done] (step) (->Ps #(q :cancel) q))}})
    (q {:grow 0
        :degree 0
        :shrink 0
        :permutation {}
        :change {}
        :freeze #{}})
    (q {:grow 0
        :degree 0
        :shrink 0
        :permutation {}
        :change {}
        :freeze #{}})
    (is (= @ps {:grow        0
                :degree      0
                :shrink      0
                :permutation {}
                :change      {}
                :freeze      #{}}))
    (step)
    (is (= (q) :step))
    (q {:grow   0
        :degree 2
        :shrink 0
        :permutation {}
        :freeze #{}
        :change {1 (fn [step done] (step) (->Ps #(q :cancel) q))}})
    (q {:grow 1
        :degree 1
        :shrink 0
        :permutation {}
        :change {0 "hello"}
        :freeze #{}})
    (is (= @ps {:grow        1
                :degree      1
                :shrink      0
                :permutation {}
                :change      {0 "hello"}
                :freeze      #{}}))
    (is (= (q) :cancel))
    (step)
    (is (= (q) :step))
    (q {:grow   0
        :degree 2
        :shrink 0
        :permutation {}
        :freeze #{}
        :change {0 (fn [step done] (step) (->Ps #(q :cancel) q))}})
    (q {:grow   1
        :degree 1
        :shrink 0
        :permutation {}
        :change {0 "hello"}
        :freeze #{}})
    (is (= @ps {:grow 1
                :degree 2
                :shrink 0
                :permutation {0 1, 1 0}
                :change {0 "hello"}
                :freeze #{}}))
    (is (= (q) :cancel))))

(deftest flush-after-crash
  (let [q (queue)
        ps ((flow (fn [step done]
                    (step)
                    (->Ps #(q :cancel)
                      (fn []
                        (done)
                        {:grow 2
                         :degree 2
                         :shrink 0
                         :permutation {}
                         :change {0 (fn [step done]
                                      (step)
                                      (->Ps #(q :cancel0)
                                        (fn []
                                          (done)
                                          (throw (error "crash0")))))
                                  1 (fn [step done]
                                      (step)
                                      (->Ps #(q :cancel1)
                                        (fn []
                                          (done)
                                          (throw (error "crash1")))))}
                         :freeze #{}}))))
            #(q :step) #(q :done))]
    (is (= (q) :step))
    (is (thrown? #?(:clj Error :cljs js/Error) @ps))
    (is (= (hash-set (q) (q) (q)) #{:cancel :cancel0 :cancel1}))
    (is (= (q) :done))))

(deftest flush-immediately
  (let [q (queue)
        ps ((flow (fn [step done]
                    (q step)
                    (step)
                    (->Ps #(q :cancel)
                      (fn []
                        (if (q)
                          {:grow        1
                           :degree      1
                           :shrink      0
                           :permutation {}
                           :change      {0 (fn [step done]
                                             (step)
                                             (q true)
                                             (->Ps #(q :cancel0)
                                               (fn []
                                                 (if (q)
                                                   (do (q step) (i/empty-diff 0))
                                                   (do (done) (throw (Cancelled.)))))))}}
                          (do (done)
                              {:grow        0
                               :degree      1
                               :shrink      1
                               :permutation {}
                               :change      {}
                               :freeze      #{}}))))))
            #(q :step) #(q :done))
        step (q)]
    (is (= (q) :step))
    (q true)
    (is (= @ps (i/empty-diff 0)))
    (let [step0 (q)]
      (step)
      (is (= (q) :step))
      (q false)
      (is (= @ps (i/empty-diff 0)))
      (is (= (q) :cancel0))
      (q false)
      (step0)
      (is (q) :done))))