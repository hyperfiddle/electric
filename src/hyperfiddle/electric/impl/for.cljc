(ns hyperfiddle.electric.impl.for
  (:require [hyperfiddle.electric.impl.gather :refer [gather]]
            [hyperfiddle.electric.impl.eventually :refer [eventually]]
            [hyperfiddle.electric.impl.runtime :as r]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as str])
  (:import missionary.Cancelled
           hyperfiddle.electric.Failure
           #?(:clj (clojure.lang MapEntry))))

(defn seq-diff [kf]
  (fn [rf]
    (let [state (object-array 4) ; 4 slots: [previous index, previous list, next index, next list]
          append (fn [^objects o k] ; add to a circular doubly linked list + update index.
                   (let [next-index (aget state (int 2))]
                     ;; add item to current index
                     (aset state (int 2) ; set next index
                       (assoc next-index k (conj (get next-index k []) o))) ; next-index new looks like {k [?o], ...}. Must map to a vector in case of keyfn collision
                     ;; append item to current list
                     (if-some [^objects next-head (aget state (int 3))] ; list is not empty
                       (let [^objects next-tail (aget next-head (int 0))]
                         (aset o (int 0) next-tail)
                         (aset o (int 1) next-head)
                         (aset next-head (int 0) o)
                         (aset next-tail (int 1) o))
                       (do (aset o (int 0) o) ; list is empty
                         (aset o (int 1) o)
                         (aset state (int 3) o)))))
          scan (fn [r x]
                 (let [k (kf x)
                       prev-index (aget state (int 0))
                       prev-head (aget state (int 1))]
                   (if-some [[o & os] (get prev-index k)]
                     (let [prev (aget o (int 0)) ; element already exists
                           next (when-not (identical? o prev) ; list is of size 1
                                  (aset prev (int 1) ; next <- prev and prev <- next
                                    (doto (aget o (int 1))
                                      (aset (int 0) prev))))
                           ;; emit change if needed
                           r (if (= x (aget o (int 2)))
                               r (rf r o (aset o (int 2) x))) ; value changed, emit a change operation
                           r (if (identical? o prev-head) ; item didn't move
                               (do (aset state (int 1) next) r) ; if item was head, move head forward
                               (rf r nil [o prev-head]))]  ; item moved, emit a move operation
                       ;; remove from previous index
                       (aset state (int 0) (assoc prev-index k os))
                       (append o k) r)
                     (let [o (object-array 3)] ; new item -> allocate 3 slots: [previous item, next item, value]
                       (append o k)
                       (-> r
                         (rf o (aset o (int 2) x)) ; emit change operation
                         (rf nil [o prev-head]) ; emit move operation
                         )))))]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r xs]
         (if (instance? Failure xs)
           (if-some [prev-head (aget state (int 1))]
             (loop [o prev-head, r r] ; iterate tail to head
               (let [o (aget o (int 0))
                     r (rf r o (aset o (int 2) xs))] ; set branch value to failure instance, then emit change operation.
                 (if (identical? o prev-head)
                   r (recur o r)))) r)
           (let [r (reduce scan r xs) ; after scan we have a new list and new index
                 r (if-some [prev-head (aget state (int 1))] ; remaining items in prev list are removals
                     (loop [o prev-head, r r] ; iterate tail to head
                       (let [o (aget o (int 0))
                             r (rf r nil [o o])] ; emit removal operation
                         (if (identical? o prev-head) ; are we at the end of the list?
                           r (recur o r)))) r)]
             (aset state (int 0) (aget state (int 2)))
             (aset state (int 1) (aget state (int 3)))
             (aset state (int 2) nil)
             (aset state (int 3) nil)
             r)))))))

(defn entry [k v]
  #?(:clj (MapEntry. k v)
     :cljs (->MapEntry k v nil)))

(tests
  (let [alice-caramail {:id "alice" :email "alice@caramail.com"}
        alice-gmail    {:id "alice" :email "alice@gmail.com"}
        bob            {:id "bob" :email "bob@yahoo.com"}
        alice-msn      {:id "alice" :email "alice@msn.com"}]
    (sequence (comp (seq-diff :id) (map entry))
      [[alice-caramail]
       [alice-gmail bob]
       [alice-gmail alice-msn bob]
       [alice-gmail bob alice-msn]
       [bob alice-msn]
       []]) :=
    [                    ; first change (initial)
     [?a alice-caramail] ; set value of ?a to be {:id "alice", :email "..."}
     [nil [?a nil]]      ; if first element is nil -> movement, if it's an object -> event on an item
                         ; second element is a pair:
                         ;  - first element is an identifier for the object we are moving
                         ;  - second element is the target where to move it, nil means append at the end
                         ; second change
     [?a alice-gmail]    ; set value of ?a again (email changed)
     [?b bob]            ; set value of ?b to bob
     [nil [?b nil]]      ; a new object appears at the end of the list
                         ; third change
     [?c alice-msn]      ; set ?c to alice-msn
     [nil [?c ?b]]       ; insert new object ?c before ?b
                         ; fourth change
     [nil [?b ?c]]       ; insert ?b before ?c (bob before 2nd alice)
                         ; Fifth change
     [nil [?b ?a]]       ; insert ?b before ?a
     [?a alice-msn]      ; set ?a to be alice-msn
     [nil [?c ?c]]       ; remove ?c
                         ; Last change
     [nil [?a ?a]]       ; drop ?a
     [nil [?b ?b]]       ; drop ?b
     ]))

(defn insert-before [tier]
  (fn [rf]
    (let [state (doto (object-array 2)
                  (aset (int 0) {})                         ;; key -> pos
                  (aset (int 1) []))]                       ;; pos -> key
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r [target anchor]]
         (let [r (let [k->p (aget state (int 0))]
                   (if (contains? k->p target)
                     r (do (aset state (int 0) (assoc k->p target (count k->p)))
                           (aset state (int 1) (conj (aget state (int 1)) target))
                           (rf r [target]))))
               k->p (aget state (int 0))
               start-position (k->p target)                 ; get start position from identifier
               anchor-position (if (= anchor target)        ; means removal
                                 (count k->p)               ; end of the vector
                                 (case anchor
                                   nil (count k->p)         ; means target is moved to the end of the vector
                                   (k->p anchor)))
               final-position (if (< start-position anchor-position)
                                (dec anchor-position) anchor-position)
               step (compare final-position start-position) ; 1, 0, or -1. In which way are we rotating? Do we iterate LTR or RTL?
               r (case step
                   0 r                                      ; if we move a before b in [a b c], a is already just before b, nothing to do.
                   (loop [ks [target], i start-position]    ; start in start-position, end in final-position
                     (let [j (+ i step)                     ; move one step to the left or right
                           k (nth (aget state (int 1)) j)
                           ks (conj ks k)]
                       (aset state (int 0) (assoc (aget state (int 0)) k i))
                       (aset state (int 1) (assoc (aget state (int 1)) i k))
                       (if (== j final-position)
                         (do (aset state (int 0) (assoc (aget state (int 0)) target j))
                             (aset state (int 1) (assoc (aget state (int 1)) j target))
                             (rf r ks)) (recur ks j)))))]
           (if (= anchor target)
             (do (aset state (int 0) (dissoc (aget state (int 0)) target))
                 (aset state (int 1) (pop (aget state (int 1))))
                 (r/move tier start-position start-position)
                 (rf r [target]))
             (do (when-not (== start-position final-position)
                   (r/move tier start-position final-position)) r))))))))

;; TODO fix the test, requires to mock a tier :/
(comment
  (sequence insert-before [[:a nil]]) :=
  [[:a]]

  (sequence insert-before [[:a nil] [:b nil] [:c :a]]) :=
  [[:a] [:b] [:c] [:c :b :a]]

  (sequence insert-before [[:a nil] [:b nil] [:c nil] [:a :a]]) :=
  [[:a] [:b] [:c] [:a :b :c] [:a]]

  (sequence insert-before [[:b nil] [:c nil] [:a :b]]) :=
  [[:b] [:c] [:a] [:a :c :b]])

(defn apply-cycle [{:keys [vals index failed] :as r} [x & ys]]
  (if-some [[y & ys] ys]
    (let [i (index x)
          v (vals i)]
      (loop [index index
             vals vals
             i i
             y y
             ys ys]
        (let [j (index y)
              index (assoc index y i)
              vals (assoc vals i (vals j))]
          (if-some [[y & ys] ys]
            (recur index vals j y ys)
            (assoc r
              :index (assoc index x j)
              :vals (assoc vals j v))))))
    (if-some [i (index x)]
      (do (assert (== (inc i) (count index)))
          (assoc r
            :failed (disj failed x)
            :index (dissoc index x)
            :vals (pop vals)))
      (assoc r
        :index (assoc index x (count index))
        :vals (conj vals nil)))))

(defn apply-change [{:keys [vals index failed] :as r} k v]
  (if-some [i (index k)] ; look up branch id (?a, ?b, ...) in [id -> position] index map
    (assoc r
      :vals (assoc vals i v)
      :failed ((if (instance? Failure v) ; if v is a Failure, store its corresponding branch position in :failed set
                 conj disj) failed k)) r))

(defn values [{:keys [vals index failed]}] ; Either Failure | [value]
  (if-some [[i] (seq (sort (mapv index failed)))]    ; Sorted because we want to report the first error in the same order as the input collection.
    (vals i)               ; error value produced by branch k. An instance of Failure.
    vals))

(defn seq-patch
  ([] {:vals [] ; vector of each branch result (result of the for)
       :index {} ; map of [branch id -> position in vector]
       :failed #{}}) ; Set of positions of branches in an error state.
  ([r] r)
  ([r diff]
   (reduce-kv apply-change
     (reduce apply-cycle r (get diff nil))
     (dissoc diff nil))))

(tests
  (reduce seq-patch (seq-patch)
    [{nil [[:a]]
      :a  {:id "alice", :email "alice@caramail.com"}}
     {nil [[:b]]
      :a  {:id "alice", :email "alice@gmail.com"}
      :b  {:id "bob", :name "bob@yahoo.com"}}
     {nil [[:c]]
      :c  {:id "alice", :email "alice@msn.com"}}
     {nil [[:c]]}])
  :=
  {:vals  [{:id "alice", :email "alice@gmail.com"}
           {:id "bob", :name "bob@yahoo.com"}]
   :index {:a 0 :b 1}
   :failed #{}})

(defn map-by "
Given a function and a continuous flow of collections, returns a continuous flow of vectors of the same size as input
collection, where values are produced by the continuous flow returned by the function when called with the continuous
flow of values matching the identity provided by key function, defaulting to identity."
  ([tier f >xs] (map-by tier identity f >xs))
  ([tier k f >xs]
   (->> >xs
     (m/eduction (seq-diff k) (map entry))
     (m/group-by key)
     (m/sample (fn [[id >x]]
                 (case id
                   nil (->> >x
                         (m/eduction
                           (map val)
                           (insert-before tier)
                           (map vector))
                         (m/relieve into)
                         (m/sample (partial hash-map id)))
                   (->> >x
                     (m/eduction (map val))
                     (m/relieve {})
                     (f)
                     (r/with tier)
                     (m/latest (partial hash-map id))))))
     (gather merge)
     (m/reductions seq-patch)
     (m/latest values))))
