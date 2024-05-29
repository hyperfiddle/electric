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

(defn slot [frame id]
  (r/->Slot frame id))

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
        p (peer {})
        f (frame p nil 0 nil nil nil)
        mp (doto (mp/create)
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
        p (peer {})
        f (frame p nil 0 nil nil)
        mp (mp/create)
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

(deftest cousin-tags
  (let [q (queue)
        p (peer {:cdef [(r/cdef 0 [] [] nil (fn [frame] (r/pure nil)))]})
        r (frame p nil 0
            (m/observe (fn [!] (! (d/empty-diff 0)) (q !) #(q :dispose))))
        f1 (frame p (slot r 0) 0 nil)
        f2 (frame p (slot r 0) 1 nil)
        mp (doto (mp/create)
             (kvs/insert! (r/tag f1 0) :foo)
             (kvs/insert! (r/tag f2 0) :bar))
        ps (mp #(q :step) #(q :done))]
    (is (= (q) :step))
    (is (= @ps (i/empty-diff 0)))
    (let [diff! (q)]
      (diff! {:grow 2
              :degree 2
              :shrink 0
              :permutation {}
              :change {0 (r/ctor :cdef 0)
                       1 (r/ctor :cdef 0)}
              :freeze #{}})
      (is (= (q) :step))
      (is (= @ps {:grow 2
                  :degree 2
                  :shrink 0
                  :permutation {}
                  :change {0 :foo, 1 :bar}
                  :freeze #{}}))
      (diff! {:grow 0
              :degree 2
              :shrink 0
              :permutation {0 1, 1 0}
              :change {}
              :freeze #{}})
      (is (= (q) :step))
      (is (= @ps {:grow 0
                  :degree 2
                  :shrink 0
                  :permutation {0 1, 1 0}
                  :change {}
                  :freeze #{}}))
      (diff! {:grow 0
              :degree 2
              :shrink 1
              :permutation {}
              :change {}
              :freeze #{}})
      (is (= (q) :step))
      (is (= @ps {:grow 0
                  :degree 2
                  :shrink 1
                  :permutation {}
                  :change {}
                  :freeze #{}}))
      (kvs/update! mp (r/tag f2 0) (constantly :baz))
      (is (= (q) :step))
      (is (= @ps {:grow 0
                  :degree 1
                  :shrink 0
                  :permutation {}
                  :change {0 :baz}
                  :freeze #{}}))
      (ps)
      (is (= (q) :step))
      (is (= (q) :dispose))
      (is (thrown? Cancelled @ps))
      (is (= (q) :done)))))