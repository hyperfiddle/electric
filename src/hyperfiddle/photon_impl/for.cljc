(ns hyperfiddle.photon-impl.for
  (:require [hyperfiddle.photon-impl.gather :refer [gather]]
            [hyperfiddle.photon-impl.eventually :refer [eventually]]
            [hyperfiddle.photon-impl.runtime :as r]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as str])
  (:import missionary.Cancelled
           hyperfiddle.photon.Failure
           #?(:clj (clojure.lang MapEntry))))

(defn seq-diff [kf]
  (fn [rf]
    (let [state (object-array 4) ; 4 slots: [previous index, previous list, next index, next list]
          append (fn [^objects o k] ; add to a circular doubly linked list + update index.
                   (let [next-index (aget state (int 2))]
                     ;; add item to current index
                     (aset state (int 2) ; set next index
                       (assoc next-index k (conj (get next-index k []) o))) ; next-index new looks like {k [?o], …}. Must map to a vector in case of keyfn collision
                     ;; append item to current list
                     (if-some [^objects next-head (aget state (int 3))] ; list is not empty
                       (let [^objects next-tail (aget next-head (int 0))]
                         (aset o (int 0) next-tail)
                         (aset o (int 1) next-head)
                         (aset next-head (int 0) o)
                         (aset next-tail (int 1) o))
                       (do (aset o (int 0) o) ; list is empty
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
                           r (if (identical? o prev-head) ; item didn't move
                               (do (aset state (int 1) next) r) ; if item was head, move head forward
                               (rf r nil [o prev-head]))  ; item moved, emit a move operation
                           ;; emit change if needed
                           r (if (= x (aget o (int 2))) r
                               (rf r o (aset o (int 2) x)) ; value changed, emit a change operation
                               )]
                       ;; remove from previous index
                       (aset state (int 0) (assoc prev-index k os))
                       (append o k) r)
                     (let [o (object-array 3)] ; new item -> allocate 3 slots: [previous item, next item, value]
                       (append o k)
                       (-> r
                         (rf nil [o prev-head]) ; emit move operation
                         (rf o (aset o (int 2) x)) ; emit change operation
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
  (sequence (comp (seq-diff :id) (map entry))
    [[{:id "alice" :email "alice@caramail.com"}]
     ;; Add bob and change email for alice
     [{:id "alice" :email "alice@gmail.com"}
      {:id "bob" :email "bob@yahoo.com"}]
     ;; Add a second alice before bob, moving bob to 3rd place
     [{:id "alice" :email "alice@gmail.com"}
      {:id "alice" :email "alice@msn.com"}
      {:id "bob" :email "bob@yahoo.com"}]
     ;; Move second alice after bob
     [{:id "alice" :email "alice@gmail.com"}
      {:id "bob" :email "bob@yahoo.com"}
      {:id "alice" :email "alice@msn.com"}]
     ;; Drop first alice
     [{:id "bob" :email "bob@yahoo.com"}
      {:id "alice" :email "alice@msn.com"}]
     ;; Drop all
     []]) :=
  [; first change (initial)
   [nil [?a nil]] ; if first element is nil -> movement, if it's an object -> event on an item 
                  ; second element is a pair:
                  ;  - first element is an identifier for the object we are moving
                  ;  - second element is the target where to move it, nil means append at the end
   [?a {:id "alice", :email "alice@caramail.com"}] ; set value of ?a to be {:id "alice", :email "…"}
   ; second change
   [?a {:id "alice", :email "alice@gmail.com"}]    ; set value of ?a again (email changed)
   [nil [?b nil]] ; a new object appears at the end of the list
   [?b {:id "bob", :email "bob@yahoo.com"}] ; set value of ?b to {:id "bob", :email "…"}
   ; third change
   [nil [?c ?b]] ; insert new object ?c before ?b
   [?c {:id "alice", :email "alice@msn.com"}] ; set ?c to {:id "alice", :email "alice@msn.com"}
   ; fourth change
   [nil [?b ?c]] ; insert ?b before ?c (bob before 2nd alice)
   ; Fifth change
   [nil [?b ?a]] ; insert ?b before ?a
   [?a {:id "alice", :email "alice@msn.com"}] ; set ?a to be {:id "alice", :email "alice@msn.com"}
   [nil [?c ?c]] ; remove ?c
   ; Last change
   [nil [?a ?a]] ; drop ?a
   [nil [?b ?b]] ; drop ?b
   ])

(defn insert-before [r x]
  ; x is either a target or an anchor
  (let [{:keys [index target]} r
        n (count index)]
    (case target
      ;; Set target or create new target
      nil (if (contains? index x) ; no target
            (assoc r :target x) ; x exist, this is a move operation. Set x as current target.
            (let [{:keys [keys vals]} r] ; this is a creation operation, allocate a slot at the end of the vector
              (assoc r
                :vals (conj vals nil) ; initial value associated to x is `nil`
                :keys (conj keys x)   ; add identifier (x) at the end
                :index (assoc index x n) ; store mapping between identifier and position
                :target x)))
      ;; Move and maybe remove target
      ;; Removal is implemented in 2 steps: 1. move target to the end of the vector, 2. shrink the vector by 1.
      (let [start-position (index target) ; get start position from identifier
            anchor-position (if (= x target) ; means removal
                              n ; end of the vector 
                              (case x
                                nil n ; means target is moved to the end of the vector
                                (index x)))
            final-position (if (< start-position anchor-position) (dec anchor-position) anchor-position)
            step (compare final-position start-position) ; 1, 0, or -1. In which way are we rotating? Do we iterate LTR or RTL?
            r (dissoc r :target)
            r (case step
                0 r ; if we move a before b in [a b c], a is already just before b, nothing to do.
                (let [{:keys [vals keys]} r
                      v (nth vals start-position)]
                  ;; TODO since we move items one by one between start-position and final-position, use a transient or equivalent.
                  (loop [vals vals, keys keys, index index, i start-position] ; start in start-position, end in final-position
                    (let [j (+ i step) ; move one step to the left or right
                          k (nth keys j)
                          vals (assoc vals i (nth vals j))
                          keys (assoc keys i k)
                          index (assoc index k i)]
                      (if (== j final-position)
                        (assoc r
                          :vals (assoc vals j v)
                          :keys (assoc keys j target)
                          :index (assoc index target j))
                        (recur vals keys index j))))))]
        (if (= x target) ; Removal, target is now at the end of the vector
          (do (r/move (inc start-position)) ; notify hook
            (-> r
              (update :vals pop) ; shrink vector by 1
              (update :keys pop)
              (update :index dissoc x)))
          (do (r/move (inc start-position) (inc final-position)) r))))))

(tests
  (reduce insert-before
    '{:vals [a b c] :keys [:a :b :c] :index {:a 0 :b 1 :c 2}}
    [:a :c]) :=
  '{:vals [b a c] :keys [:b :a :c] :index {:b 0 :a 1 :c 2}}

  (reduce insert-before
    '{:vals [a b c] :keys [:a :b :c] :index {:a 0 :b 1 :c 2}}
    [:c :a]) :=
  '{:vals [c a b] :keys [:c :a :b] :index {:c 0 :a 1 :b 2}}

  (reduce insert-before
    '{:vals [a b c] :keys [:a :b :c] :index {:a 0 :b 1 :c 2}}
    [:a :a]) :=
  '{:vals [b c] :keys [:b :c] :index {:b 0, :c 1}}

  (reduce insert-before
    '{:vals [b c] :keys [:b :c] :index {:b 0 :c 1}}
    [:a :b]) :=
  '{:vals [nil b c] :keys [:a :b :c] :index {:a 0, :b 1, :c 2}})

(defn change [{:keys [index] :as r} k v]
  (if-some [i (index k)] ; look up branch id (?a, ?b, …) in [id -> position] index map
    (-> r
      (update :vals assoc i v)
      (update :failed
        (if (instance? Failure v) ; if v is a Failure, store its corresponding branch position in :failed set
          conj disj) i)) r))

(defn values [{:keys [vals failed]}] ; Either Failure | [value]
  (if-some [[i] (seq failed)]
    (vals i) ; error value produced by branch at position i. An instance of Failure.
    vals))

(defn seq-patch
  ([] {:vals [] ; vector of each branch result (result of the for)
       :keys [] ; vector of the same size as :vals, containing branch identifiers (?a, ?b, ?c produced by seq-diff)
       :index {} ; map of [branch id -> position in vector], inverse of :keys
       :failed (sorted-set)} ; Set of positions of branches in an error state. Sorted because we want to report the first error in the same order as the input collection.
      )
  ([r] r)
  ([r diff]
   (reduce-kv change
     (reduce insert-before r (get diff nil))
     (dissoc diff nil))))

(tests
  (reduce seq-patch (seq-patch)
    [{nil [:a nil]
      :a  {:id "alice", :email "alice@caramail.com"}}
     {nil [:b nil]
      :a {:id "alice", :email "alice@gmail.com"}
      :b {:id "bob", :name "bob@yahoo.com"}}
     {nil [:c nil]
      :c {:id "alice", :email "alice@msn.com"}}
     {nil [:c :c]}])
  :=
  {:vals  [{:id "alice", :email "alice@gmail.com"}
           {:id "bob", :name "bob@yahoo.com"}]
   :keys  [:a :b]
   :index {:a 0 :b 1}
   :failed #{}})

(defn map-by "
Given a function and a continuous flow of collections, returns a continuous flow of vectors of the same size as input
collection, where values are produced by the continuous flow returned by the function when called with the continuous
flow of values matching the identity provided by key function, defaulting to identity."
  ([f >xs] (map-by identity f >xs))
  ([k f >xs]
   (->> >xs
     (m/eduction (seq-diff k) (map entry))
     (m/group-by key)
     (m/zip (fn [[id >x]]
              (let [>x-val (m/zip val >x)]
                (case id
                  nil (m/zip (partial hash-map id)
                        (m/relieve into >x-val))
                  (m/latest (partial hash-map id)
                    (f (m/relieve {} >x-val)))))))
     (gather merge)
     (m/reductions seq-patch)
     (m/latest values))))

(tests

  (let [!xs (atom [])
        it ((map-by :id
              (partial m/latest (fn [x] (if (r/failure x) x (update x :email str/upper-case))))
              (m/watch !xs)) #() #())]
    @it := []
    (swap! !xs conj {:id "alice" :email "alice@caramail.com"})
    @it := [{:id "alice" :email "ALICE@CARAMAIL.COM"}]
    (swap! !xs assoc 0 {:id "alice" :email "alice@gmail.com"})
    @it := [{:id "alice" :email "ALICE@GMAIL.COM"}]
    (swap! !xs conj {:id "bob" :email "bob@yahoo.com"})
    (let [x @it]
      x := [{:id "alice" :email "ALICE@GMAIL.COM"}
            {:id "bob" :email "BOB@YAHOO.COM"}]
      (swap! !xs (comp vec reverse))
      (let [y @it]
        (identical? (get x 0) (get y 1)) := true
        (identical? (get x 1) (get y 0)) := true
        (swap! !xs pop)
        @it := [{:id "bob" :email "BOB@YAHOO.COM"}]))))
