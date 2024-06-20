(ns hyperfiddle.electric.impl.mount-point-test
  (:require [hyperfiddle.incseq :as i]
            [hyperfiddle.incseq.diff-impl :as d]
            [missionary.core :as m]
            [hyperfiddle.kvs :as kvs]
            [hyperfiddle.electric.impl.runtime-de :as r]
            [hyperfiddle.electric.impl.mount-point :as mp]
            [clojure.test :refer [deftest is]])
  #?(:clj (:import (java.util LinkedList)
                   missionary.Cancelled)))

(defn peer [defs]
  (r/->Peer :client defs nil nil nil nil nil))

(defn frame [peer slot rank & tags]
  (let [tags-array (object-array (count tags))
        frame (r/->Frame peer slot rank nil nil nil tags-array nil)]
    (reduce (fn [i tag]
              (when tag
                (aset tags-array i
                  (r/create-call
                    (r/->Slot frame i)
                    :client (r/effect tag))))
              (inc i)) 0 tags) frame))

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

(deftest sibling-tags
  (let [q (queue)
        _ ((r/peer (fn [_] #()) :client
             {:root (fn ([] {0 (r/ctor :root 0)})
                      ([idx]
                       (case idx
                         0 (r/cdef 0 [] [nil nil nil] nil (fn [frame] (q frame) (r/pure nil))))))}
             :root)
            #(q :peer-step) #(q :peer-done))
        f (q)
        mp (doto (mp/create (r/frame-peer f))
             (kvs/insert! (r/tag f 0) :foo)
             (kvs/insert! (r/tag f 1) :bar)
             (kvs/insert! (r/tag f 2) :baz))
        ps (mp #(q :step) #(q :done))]
    (is (= (q) :step))
    (is (= @ps {:grow 3
                :degree 3
                :shrink 0
                :permutation {}
                :change {0 :foo, 1 :bar, 2 :baz}
                :freeze #{}}))
    (kvs/update! mp (r/tag f 1) (constantly :BAR))
    (is (= (q) :step))
    (kvs/remove! mp (r/tag f 0))
    (is (= @ps {:grow 0
                :degree 3
                :shrink 1
                :permutation {0 1, 1 2, 2 0}
                :change {0 :BAR}
                :freeze #{}}))
    (kvs/remove! mp (r/tag f 1))
    (is (= (q) :step))
    (kvs/remove! mp (r/tag f 2))
    (is (= @ps {:grow 0
                :degree 2
                :shrink 2
                :permutation {}
                :change {}
                :freeze #{}}))
    (ps)
    (is (= (q) :step))
    (is (thrown? Cancelled @ps))
    (is (= (q) :done))))

(deftest sibling-tags-insert-after-read
  (let [q (queue)
        _ ((r/peer (fn [_] #()) :client
             {:root (fn ([] {0 (r/ctor :root 0)})
                      ([idx]
                       (case idx
                         0 (r/cdef 0 [] [nil nil] nil (fn [frame] (q frame) (r/pure nil))))))}
             :root)
           #(q :peer-step) #(q :peer-done))
        f (q)
        mp (mp/create (r/frame-peer f))
        ps (mp #(q :step) #(q :done))]
    (is (= (q) :step))
    (is (= @ps (d/empty-diff 0)))
    (kvs/insert! mp (r/tag f 0) :foo)
    (kvs/insert! mp (r/tag f 1) :bar)
    (is (= (q) :step))
    (is (= @ps {:grow   2
                :degree 2
                :shrink 0
                :permutation {}
                :change {0 :foo
                         1 :bar}
                :freeze #{}}))))

(deftest cousin-tags-insert-after-read
  (let [q (queue)
        _ ((r/peer (fn [_] #()) :client
             {:root (fn ([] {0 (r/ctor :root 0)})
                      ([idx]
                       (case idx
                         0 (r/cdef 0 [] [nil] nil
                             (fn [frame]
                               (q frame)
                               (r/define-call frame 0
                                 (r/effect (m/observe
                                             (fn [!]
                                               (! {:grow        2
                                                   :degree      2
                                                   :shrink      0
                                                   :permutation {}
                                                   :change      {0 (r/ctor :root 1)
                                                                 1 (r/ctor :root 1)}
                                                   :freeze      #{}})
                                               #(q :dispose)))))
                               (r/call frame 0)))
                         1 (r/cdef 0 [] [nil] nil
                             (fn [frame]
                               (q frame)
                               (r/pure nil))))))}
           :root)
         #(q :peer-step) #(q :peer-done))
        f (q)
        f1 (q)
        f2 (q)
        mp (mp/create (r/frame-peer f))
        ps (mp #(q :step) #(q :done))]
    (is (= (q) :step))
    (is (= @ps (i/empty-diff 0)))
    (kvs/insert! mp (r/tag f1 0) :foo)
    (kvs/insert! mp (r/tag f2 0) :bar)
    (is (= (q) :step))
    (is (= @ps {:grow 2
                :degree 2
                :shrink 0
                :permutation {}
                :change {1 :bar
                         0 :foo}
                :freeze #{}}))))