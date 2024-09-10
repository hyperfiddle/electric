(ns hyperfiddle.incseq.items-eager-impl-test
  (:require
   [clojure.test :as t]
   [contrib.assert :as ca]
   [contrib.data :refer [->box]]
   [hyperfiddle.incseq.diff-impl :as d]
   [hyperfiddle.incseq.items-eager-impl :as items]
   [hyperfiddle.incseq.flow-protocol-enforcer :as fpe]
   [missionary.core :as m])
  (:import #?(:clj [clojure.lang ExceptionInfo IDeref IFn])
           [missionary Cancelled]))

(defn ->queue
  ([] #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))
  ([& args] (into (->queue) args)))

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

(defn spawn-ps
  ([q] (spawn-ps q (->box (fn [_step _done] (q)))))
  ([q <transfer-fn>] (spawn-ps q <transfer-fn> (->box (fn [_step _done] (q :input-cancel)))))
  ([q <transfer-fn> <cancel-fn>]
   ((fpe/flow "i/items" (items/flow (fn [step done]
                            (q [step done])
                            (step)
                            (reify
                              IFn (#?(:clj invoke :cljs -invoke) [_] ((<cancel-fn>) step done))
                              IDeref (#?(:clj deref :cljs -deref) [_] ((<transfer-fn>) step done))))))
    #(q :items-step) #(q :items-done))))

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

(t/deftest shrink-idle-item-ps
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
        _                  (t/is (= :item0-done (q)))
        _                  (t/is (= :items-step (q)))
        _                  (t/is (= shrink1 @items))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest shrink-stepped-item-ps
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
        _                  (t/is (= :foo @item0))
        _                  (t/is (= :item0-done (q)))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest dead-item-ps-returns-last-value-and-terminates
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

(t/deftest dead-item-ps-cancelled-throws-and-terminates
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
        _     (t/is (= (assoc (d/empty-diff 2) :grow 2) (assoc diff :change {})))
        _     (t/is (= 2 (count (:change diff))))
        _     (t/is (= :items-done (q)))
        _     (q ::none)
        _     (t/is (= ::none (q)))]))

(t/deftest input-terminate-during-transfer
  (let [q     (->mq)
        items ((items/flow (m/seed [{:grow 1, :degree 1, :shrink 0, :change {0 :foo}, :permutation {}, :freeze #{}}]))
               #(q :items-step) #(q :items-done))
        _     (t/is (= :items-step (q)))
        diff  @items
        _     (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        _     (t/is (= 1 (count (:change diff))))
        _     (t/is (= :items-done (q)))
        _     (q ::none)
        _     (t/is (= ::none (q)))]))

(t/deftest input-terminate-when-idle
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [_in-step in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        _                  (t/is (= 1 (count (:change diff))))
        _                  (in-done)
        _                  (t/is (= :items-done (q)))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest input-terminate-when-stepped
  (let [q                 (->mq)
        _                 (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items             (spawn-ps q)
        [in-step in-done] (q)
        _                 (t/is (= :items-step (q)))
        diff              @items
        _                 (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        _                 (t/is (= 1 (count (:change diff))))
        _                 (q (assoc (d/empty-diff 1) :grow 1 :change {0 :bar}))
        _                 (in-step)
        _                 (t/is (= :items-step (q)))
        _                 (in-done)
        _                 (q ::none)
        _                 (t/is (= ::none (q)))
        _diff             @items
        _                 (t/is (= :items-done (q)))
        _                 (q ::none)
        _                 (t/is (= ::none (q)))]))

(t/deftest failure-on-first-transfer
  (let [q                   (->mq)
        items               (spawn-ps q (->box (fn [_step done] (done) (throw (ex-info "boom" {})))))
        [_in-step _in-done] (q)
        _                   (t/is (= :input-cancel (q)))
        _                   (t/is (= :items-step (q)))
        _                   (t/is (thrown? ExceptionInfo @items))
        _                   (t/is (= :items-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest failure-on-non-first-transfer
  (let [q                  (->mq)
        <transfer-fn>      (->box (fn [_step _done] (d/empty-diff 0)))
        items              (spawn-ps q <transfer-fn>)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        _                  (t/is (= (d/empty-diff 0) @items))
        _                  (<transfer-fn> (fn [_step done] (done) (throw (ex-info "boom" {}))))
        _                  (in-step)
        _                  (t/is (= :input-cancel (q)))
        _                  (t/is (= :items-step (q)))
        _                  (t/is (thrown? ExceptionInfo @items))
        _                  (t/is (= :items-done (q)))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(defn consume-calling [f*]
  (let [<f*> (->box (seq f*))]
    (fn [step done]
      ((ca/is (<f*> first next) some? "overconsumed") step done))))

(t/deftest failure-on-reentrant-transfer
  (let [q                   (->mq)
        <transfer-fn>       (->box (consume-calling [(fn [step _] (step) (d/empty-diff 0))
                                                     (fn [_ done] (done) (throw (ex-info "boom" {})))]))
        items               (spawn-ps q <transfer-fn>)
        [_in-step _in-done] (q)
        _                   (t/is (= :input-cancel (q)))
        _                   (t/is (= :items-step (q)))
        _                   (t/is (thrown? ExceptionInfo @items))
        _                   (t/is (= :items-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest failure-after-cancellation
  (let [q                   (->mq)
        <transfer-fn>       (->box (consume-calling [(fn [_ _] (d/empty-diff 0))
                                                     (fn [_ done] (done) (throw (ex-info "boom" {})))]))
        <cancel-fn>         (->box (fn [_step _done]))
        items               (spawn-ps q <transfer-fn> <cancel-fn>)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        _                   (t/is (= (d/empty-diff 0) @items))
        _                   (items)
        _                   (t/is (= :items-step (q)))
        _                   (t/is (thrown? Cancelled @items)) ; is this OK or should the ExInfo come out
        _                   (t/is (= :items-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest grow
  (let [q                   (->mq)
        n                   (inc items/+initial-item-size+)
        _                   (q (assoc (d/empty-diff n) :grow n :change (zipmap (range n) (repeat :foo)))) ; what input will return on transfer
        items               (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        diff                @items
        _                   (t/is (= 9 (count (:change diff))))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest double-cancellation-stepped
  (let [q                   (->mq)
        _                   (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items               (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        _                   (items)
        _                   (t/is (= :input-cancel (q)))
        _                   (items)
        _                   (t/is (= :input-cancel (q)))
        _                   (t/is (thrown? Cancelled @items))
        _                   (t/is (= :items-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest double-cancellation-idle
  (let [q                   (->mq)
        _                   (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items               (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        _diff               @items
        _                   (items)
        _                   (t/is (= :input-cancel (q)))
        _                   (t/is (= :items-step (q)))
        _                   (items)
        _                   (t/is (= :input-cancel (q)))
        _                   (t/is (thrown? Cancelled @items))
        _                   (t/is (= :items-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest cancel-after-done
  (let [q                   (->mq)
        _                   (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items               (spawn-ps q)
        [_in-step _in-done] (q)
        _                   (t/is (= :items-step (q)))
        _diff               @items
        _                   (items)
        _                   (t/is (= :input-cancel (q)))
        _                   (t/is (= :items-step (q)))
        _                   (t/is (thrown? Cancelled @items))
        _                   (t/is (= :items-done (q)))
        _                   (items)
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest cancel-after-done-normally
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [_in-step in-done] (q)
        _                  (t/is (= :items-step (q)))
        _diff              @items
        _                  (in-done)
        _                  (t/is (= :items-done (q)))
        _                  (items)
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest item-ps-double-cancellation-idle
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
        _                   (item0)
        _                   (t/is (thrown? Cancelled @item0))
        _                   (t/is (= :item0-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest item-ps-double-cancellation-stepped
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
        _                   (item0)
        _                   (t/is (thrown? Cancelled @item0))
        _                   (t/is (= :item0-done (q)))
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest item-ps-cancel-after-done
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
        _                   (item0)
        _                   (q ::none)
        _                   (t/is (= ::none (q)))]))

(t/deftest dead-item-ps-cancel-after-done
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
        _                  (item0)
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest dead-item-ps-cancel-after-throw
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
        _                  (item0)
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest change-index-respects-permutation
  (let [q                  (->mq)
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo})) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= (assoc (d/empty-diff 1) :grow 1) (assoc diff :change {})))
        _                  (q {:grow 1, :degree 2, :shrink 1, :permutation {0 1, 1 0}, :change {0 :bar}})
        _                  (in-step)
        _                  (t/is (= :items-step (q)))
        diff               @items
        _                  (t/is (= 0 (-> diff :change keys first)))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest input-must-be-initialized
  (let [q                  (->mq)
        items ((items/flow (fn [step done]
                             (q [step done])
                             (reify
                               IFn (#?(:clj invoke :cljs -invoke) [_] (q :input-cancel))
                               IDeref (#?(:clj deref :cljs -deref) [_] (q)))))
               #(q :items-step) #(q :items-done))
        _ (t/is (= :input-cancel (q)))
        _ (t/is (thrown? ExceptionInfo @items))]))

(t/deftest input-transfer-decrements-on-non-needed-diff
  (let [q                  (->mq)
        _                  (q (d/empty-diff 0)) ; what input will return on transfer
        items              (spawn-ps q)
        [in-step _in-done] (q)
        _                  (t/is (= :items-step (q)))
        _                  (t/is (= (d/empty-diff 0) @items))
        _                  (q (d/empty-diff 0))
        _                  (in-step)
        _                  (q ::none)
        _                  (t/is (= ::none (q)))
        _                  (q (assoc (d/empty-diff 1) :grow 1 :change {0 :foo}))
        _                  (in-step)
        _                  (t/is (= (-> (d/empty-diff 1) (assoc :grow 1) (dissoc :change)) (dissoc @items :change)))
        _                  (t/is (= :items-step (q)))
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

(t/deftest orphaned-item-ps-doesnt-step-on-cancel
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
        _                  (q (assoc (d/empty-diff 1) :shrink 1))
        _                  (in-step)
        _                  (t/is (= :item0-done (q)))
        _                  (t/is (= :items-step (q)))
        _                  (t/is (= (assoc (d/empty-diff 1) :shrink 1) @items))
        _                  (item0)
        _                  (q ::none)
        _                  (t/is (= ::none (q)))]))

;; missing tests
;; - double transfer (optional)
;;   - item-ps
;;   - dead-item-ps
;;   - items
;; - thread safety
;; - freeze
