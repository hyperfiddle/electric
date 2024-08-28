(ns hyperfiddle.incseq.items-eager-impl-test
  (:require
   [clojure.test :as t]
   [contrib.assert :as ca]
   [hyperfiddle.incseq.diff-impl :as d]
   [hyperfiddle.incseq.items-eager-impl :as items]
   [missionary.core :as m])
  (:import #?(:clj [clojure.lang ExceptionInfo IDeref IFn])
           [missionary Cancelled]))

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

(defn spawn-ps [q]
  ((items/flow (fn [step done]
                 (q [step done])
                 (step)
                 (reify
                   IFn (#?(:clj invoke :cljs -invoke) [_] (q :input-cancel))
                   IDeref (#?(:clj deref :cljs -deref) [_] (q)))))
   #(q :items-step) #(q :items-done)))

(t/deftest spawn
  (let [q                   (->mq)
        _                   (q (d/empty-diff 0)) ; what input will return on transfer
        ps                  (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        _                   (t/is (= (d/empty-diff 0) @ps))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest one-item
  (let [q                   (->mq)
        _                   (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items               (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        diff                @items
        _                   (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0               ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                   (t/is (= :item0-step (q)))
        _                   (t/is (= :foo @item0))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest one-item-change
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0              ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                  (t/is (= :item0-step (q)))
        _                  (t/is (= :foo @item0))
        _                  (q (assoc (d/empty-diff 1) :change {0 :bar}))
        _                  (in-step)
        _                  (t/is (= :item0-step (q)))
        _                  (t/is (= :bar @item0))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest one-item-dedupes
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0              ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                  (t/is (= :item0-step (q)))
        _                  (t/is (= :foo @item0))
        _                  (q (assoc (d/empty-diff 1) :change {0 :foo}))
        _                  (in-step)
        _                  (q ::none)                    ; :foo = :foo, so we skipped
        _                  (t/is (= ::none (q)))]))

(t/deftest two-items
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0              ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                  (t/is (= :item0-step (q)))
        _                  (t/is (= :foo @item0))
        _                  (q {:grow 1, :degree 2, :shrink 0, :permutation {}, :freeze #{}, :change {1 :bar}})
        _                  (in-step)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= {:grow 1, :degree 2, :shrink 0, :permutation {}, :freeze #{}} (dissoc diff :change)))
        item1              ((-> diff :change (get 1)) #(q :item1-step) #(q :item1-done))
        _                  (t/is (= :item1-step (q)))
        _                  (t/is (= :bar @item1))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest item-is-latest
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0              ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                  (t/is (= :item0-step (q)))
        _                  (q (assoc (d/empty-diff 1) :change {0 :bar}))
        _                  (in-step)
        _                  (t/is (= :bar @item0))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest two-item-processes
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0-ps0          ((-> diff :change (get 0)) #(q :item0-ps0-step) #(q :item0-ps0-done))
        _                  (t/is (= :item0-ps0-step (q)))
        item0-ps1          ((-> diff :change (get 0)) #(q :item0-ps1-step) #(q :item0-ps1-done))
        _                  (t/is (= :item0-ps1-step (q)))
        _                  (t/is (= :foo @item0-ps1))     ; ps1 reads, ps0 didn't
        _                  (q (assoc (d/empty-diff 1) :change {0 :bar}))
        _                  (in-step)
        _                  (t/is (= :item0-ps1-step (q))) ; ps1 steps because it already transferred
        _                  (t/is (= :bar @item0-ps0))     ; ps0 transfers latest
        _                  (t/is (= :bar @item0-ps1))     ; ps1 transfers
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest permutation
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 2) :grow 2 :change {0 :foo, 1 :bar})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 2) :grow 2) (assoc diff :change {})))
        item0              ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                  (t/is (= :item0-step (q)))
        _                  (t/is (= :foo @item0))
        item1              ((-> diff :change (get 1)) #(q :item1-step) #(q :item1-done))
        _                  (t/is (= :item1-step (q)))
        _                  (t/is (= :bar @item1))
        perm               (assoc (d/empty-diff 2) :permutation {0 1, 1 0})
        _                  (q perm)
        _                  (in-step)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= perm diff))
        _                  (q (assoc (d/empty-diff 2) :change {0 :baz}))
        _                  (in-step)
        _                  (t/is (= :item1-step (q))) ; change on 0 means item1 after permutation
        _                  (t/is (= :baz @item1))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest shrink-terminates-idle-item-ps
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0              ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                  (t/is (= :item0-step (q)))
        _                  (t/is (= :foo @item0))
        shrink1            (assoc (d/empty-diff 1) :shrink 1)
        _                  (q shrink1)
        _                  (in-step)
        _                  (t/is (= :item0-step (q)))
        _                  (t/is (= :items-step (q)))
        _                  (t/is (= shrink1 @items))
        _                  (t/is (thrown? Cancelled @item0))
        _                  (t/is (= :item0-done (q)))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest shrink-terminates-stepped-item-ps
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0              ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                  (t/is (= :item0-step (q)))
        shrink1            (assoc (d/empty-diff 1) :shrink 1)
        _                  (q shrink1)
        _                  (in-step)
        _                  (t/is (= :items-step (q)))
        _                  (t/is (= shrink1 @items))
        _                  (t/is (thrown? Cancelled @item0))
        _                  (t/is (= :item0-done (q)))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest item-spawned-after-shrink-returns-last-value-and-terminates
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0-flow         (-> diff :change (get 0))
        shrink1            (assoc (d/empty-diff 1) :shrink 1)
        _                  (q shrink1)
        _                  (in-step)
        _                  (t/is (= :items-step (q)))
        _                  (t/is (= shrink1 @items))
        item0              (item0-flow #(q :item0-step) #(q :item0-done))
        _                  (t/is (= :item0-step (q)))
        _                  (t/is (= :foo @item0))
        _                  (t/is (= :item0-done (q)))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest item-spawned-after-shrink-and-cancelled-throws-and-terminates
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0-flow         (-> diff :change (get 0))
        shrink1            (assoc (d/empty-diff 1) :shrink 1)
        _                  (q shrink1)
        _                  (in-step)
        _                  (t/is (= :items-step (q)))
        _                  (t/is (= shrink1 @items))
        item0              (item0-flow #(q :item0-step) #(q :item0-done))
        _                  (t/is (= :item0-step (q)))
        _                  (item0)
        _                  (t/is (thrown? Cancelled @item0))
        _                  (t/is (= :item0-done (q)))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest item-ps-cancellation-idle
  (let [q                   (->mq)
        _                   (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items               (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        diff                @items
        _                   (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0               ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                   (t/is (= :item0-step (q)))
        _                   (t/is (= :foo @item0))
        _                   (item0)
        _                   (t/is (= :item0-step (q)))
        _                   (t/is (thrown? Cancelled @item0))
        _                   (t/is (= :item0-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest item-ps-cancellation-stepped
  (let [q                   (->mq)
        _                   (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items               (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        diff                @items
        _                   (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        item0               ((-> diff :change (get 0)) #(q :item0-step) #(q :item0-done))
        _                   (t/is (= :item0-step (q)))
        _                   (item0)
        _                   (t/is (thrown? Cancelled @item0))
        _                   (t/is (= :item0-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest cancellation-idle
  (let [q                   (->mq)
        _                   (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items               (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        diff                @items
        _                   (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        _                   (items)
        _                   (t/is (= :input-cancel (q)))
        _                   (t/is (= :items-step (q)))
        _                   (t/is (thrown? Cancelled @items))
        _                   (t/is (= :items-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest cancellation-stepped
  (let [q                   (->mq)
        _                   (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items               (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        _                   (items)
        _                   (t/is (= :input-cancel (q)))
        _                   (t/is (thrown? Cancelled @items))
        _                   (t/is (= :items-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest double-input-step
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        _                  (q (assoc (d/empty-diff 2) :grow 1 :change {1 :bar}))
        _                  (in-step)
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 2) :grow 2) (assoc diff :change {})))
        _                  (t/is (= 2 (count (:change diff))))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest reentrant-transfer
  (let [q     (->mq)
        items ((items/flow (m/seed [{:grow 1, :degree 1, :shrink 0, :change {0 :foo}, :permutation {}, :freeze #{}}
                                    {:grow 1, :degree 2, :shrink 0, :change {1 :bar}, :permutation {}, :freeze #{}}]))
               #(q :items-step) #(q :items-done))
        _     (t/is (= :items-step (q)))
        diff  @items
        _     (t/is (= {:grow 2, :degree 2, :shrink 0, :change {}, :permutation {}, :freeze #{}}
                      (assoc diff :change {})))
        _     (t/is (= 2 (count (:change diff))))
        _     (q ::none)
        _     (t/is (= ::none (q)))]))

;; missing tests
;; - items reentrant transfer
;; - input terminate
;; - failures
;; - double cancel before termination
;;   - item-ps
;;   - dead-item-ps
;;   - items
;; - double cancel after termination
;;   - item-ps
;;   - dead-item-ps
;;   - items
;; - double transfer
;;   - item-ps
;;   - dead-item-ps
;;   - items
;; - item* grow
;; - thread safety
