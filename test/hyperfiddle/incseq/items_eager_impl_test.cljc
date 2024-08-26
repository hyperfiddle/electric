(ns hyperfiddle.incseq.items-eager-impl-test
  (:require
   [clojure.test :as t]
   [contrib.assert :as ca]
   [hyperfiddle.incseq.diff-impl :as d]
   [hyperfiddle.incseq.items-eager-impl :as items])
  #?(:clj (:import
           [clojure.lang ExceptionInfo IDeref IFn])))

(defn ->queue
  ([] #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))
  ([& args] (into (->queue) args)))

(defn ->box
  ([] (->box nil))
  ([init] (let [o (doto (object-array 1) (aset (int 0) init))]
            (fn ([] (aget o (int 0)))  ([v] (aset o (int 0) v))))))

(defn ->mq []
  (let [box (->box (->queue))]
    (fn
      ([] (let [q (box)] (ca/is q seq "empty test queue") (box (pop q)) (peek q)))
      ([v] (box (conj (box) v))))))

(t/deftest queue-test
  (let [q (->mq)]
    (q 1) (t/is (= 1 (q)))
    (q 2) (q 3) (t/is (= 2 (q))) (t/is (= 3 (q)))
    (t/is (thrown? ExceptionInfo (q)))))

(t/deftest spawn
  (let [q (->mq)
        _ (q (d/empty-diff 0))          ; what input will return on transfer
        ps ((items/flow (fn [step done]
                          (q [step done])
                          (step)
                          (reify
                            IFn (#?(:clj invoke :cljs -invoke) [_] (q :input-cancel))
                            IDeref (#?(:clj deref :cljs -deref) [_] (q)))))
            #(q :items-step) #(q :items-done))
        ;; transfer (fn transfer [diff] (q diff) @ps)
        [_input-step _input-done] (q)
        _ (t/is (= :items-step (q)))
        _ (t/is (= @ps (d/empty-diff 0)))]))
