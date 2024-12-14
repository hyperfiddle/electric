(ns contrib.missionary-contrib
  "staging area, to be considered for missionary inclusion?"
  (:require [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]))

(defn mix [& flows] (m/ap (m/?> (m/?> (count flows) (m/seed flows)))))

#?(:clj (defn iterator-consumer "blocking iterable pattern"
          [^java.util.Iterator it]
          ; why not one thread tied to the iterator extent?
          ; (future (while (.hasNext it) (! (.next it))))
          (m/ap
            (loop []
              (if (m/? (m/via m/blk (.hasNext it)))
                (m/amb (m/? (m/via m/blk (.next it))) (recur))
                (m/amb))))))

#?(:clj (defn seq-consumer [xs] ; xs is iterable
          (m/ap
            (loop [xs xs]
              (if (m/? (m/via m/blk (seq xs)))
                (m/amb (m/? (m/via m/blk (first xs))) (recur (rest xs)))
                (m/amb))))))

#?(:clj
   (tests
     (def !it (.iterator (.keySet {:a 1, :b 2, :c 3, :d 4})))
     (->> (iterator-consumer !it)
          (m/eduction (take 3))
          (m/reduce conj []) m/?)
     := [:a :b :c]

     ; careful, Java iterator is stateful

     (def xs (iterator-seq (.iterator (.keySet {:a 1, :b 2, :c 3, :d 4}))))
     (take 3 xs) := [:a :b :c]

     (->> (seq-consumer xs)
          (m/eduction (take 3))
          (m/reduce conj []) m/?)
     := [:a :b :c]))

(defn poll-task
  "derive discrete flow from succession of polled values from a task (or mbox)"
  [task]
  #_(m/ap (m/? (m/?> (m/seed (repeat mbox)))))
  (m/ap
    (loop [v (m/? task)]
      (m/amb v (recur (m/? task))))))

(defn document
  "compare (document log) to (d/entity db eid). if a datomic txn is [op eid a v], 
log here is [op a v], or in other words, there is only one entity (the `eid` is 
constant) so we are left with not an entity but a document."
  [>txs]
  (m/reductions (fn [m [op a v]] ; or is the a actually e?
                  (case op
                    ::add (assoc m a v)
                    ::retract (dissoc m a))) {} >txs))

(defn throttle [dur >in]
  (m/ap
    (let [x (m/?> (m/relieve {} >in))]
      (m/amb x (do (m/? (m/sleep dur)) (m/amb))))))

(defn delay-flow [>x]
  (->> (m/reductions (fn [[_ b] nx] [b nx]) [] >x)
    (m/eduction (map second))))