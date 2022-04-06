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

(defn append [y]
  (fn [rf]
    (fn
      ([] (rf))
      ([r] (-> r (rf y) (unreduced) (rf)))
      ([r x] (rf r x)))))

(defn diffs [- z]
  (fn [rf]
    (let [state (doto (object-array 1)
                  (aset (int 0) z))]
      (fn
        ([r] (rf r))
        ([r x]
         (let [y (aget state (int 0))]
           (aset state (int 0) x)
           (rf r (- x y))))))))

(def map-diff
  "Computes the diff of given two maps."
  (partial
    reduce-kv
    (fn [diff k prev]
      (let [curr (get diff k ::done)]
        (case curr
          ::done (assoc diff k ::done)
          (if (= prev curr)
            (dissoc diff k)
            diff))))))

(def map-patch
  "Patches given map with given diff."
  (partial
    reduce-kv
    (fn [m k v]
      (case v
        ::done (dissoc m k)
        (assoc m k v)))))

(defn map-vals "
Given a function and a continuous flow of maps, returns a continuous flow of maps with the same keyset as input map,
where values are produced by the continuous flow returned by the function when called with the key and the continuous
flow of values matching that key in the input map.
" [f >m]
  (->> >m
    (m/eduction (diffs map-diff {}) cat)
    (m/group-by key)
    (m/eduction
      (map (fn [[k >x]]
             (->> >x
               (m/eduction
                 (map val)
                 (take-while (complement #{::done})))
               (m/relieve {})
               (f k)
               (eventually ::done)
               (m/latest (partial hash-map k))))))
    (gather merge)
    (m/reductions map-patch {})))

(tests
  (let [!ms (atom {})
        it ((map-vals
              (fn [_k >v] (m/latest inc >v))
              (m/watch !ms))
            #() #())]
    @it := {}
    (swap! !ms assoc :a 1)
    @it := {:a 2}
    (swap! !ms dissoc :a)
    @it := {}
    (swap! !ms assoc :a 1)
    (swap! !ms assoc :a 2)
    @it := {:a 3}))

(def val-first (partial reduce-kv (fn [r _ m] (reduce (fn [r [id]] (conj r id)) r m)) []))

(def into-vec (fnil into []))

(defn seq-diff [kf]
  (fn [rf]
    (let [state (object-array 4)
          append (fn [^objects o k]
                   (let [c (aget state (int 2))]
                     ;; add item to current index
                     (aset state (int 2)
                       (assoc c k (conj (get c k []) o)))
                     ;; append item to current list
                     (if-some [^objects h (aget state (int 3))]
                       (let [^objects t (aget h (int 1))]
                         (aset o (int 0) t)
                         (aset o (int 1) h)
                         (aset h (int 0) o)
                         (aset t (int 1) o))
                       (do (aset o (int 0) o)
                           (aset o (int 1) o)
                           (aset state (int 3) o)))))
          scan (fn [r x]
                 (let [k (kf x)
                       p (aget state (int 0))
                       h (aget state (int 1))]
                   (if-some [[o & os] (get p k)]
                     (let [prev (aget o (int 0))
                           next (when-not (identical? o prev)
                                  (aset prev (int 1)
                                    (doto (aget o (int 1))
                                      (aset (int 0) prev))))
                           ;; if item was head, move forward
                           r (if (identical? o h)
                               (do (aset state (int 1) next) r)
                               (rf r nil [o h]))
                           ;; emit change if needed
                           r (if (= x (aget o (int 2)))
                               r (rf r o (aset o (int 2) x)))]
                       ;; remove from previous index
                       (aset state (int 0) (assoc p k os))
                       (append o k) r)
                     (let [o (object-array 3)]
                       (append o k)
                       (-> r
                         (rf nil [o h])
                         (rf o (aset o (int 2) x)))))))]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r xs]
         ;; TODO failures
         (let [r (reduce scan r xs)
               r (if-some [h (aget state (int 1))]
                   (loop [o h, r r]
                     (let [o (aget o (int 0))
                           r (rf r nil [o o])]
                       (if (identical? o h)
                         r (recur o r)))) r)]
           (aset state (int 0) (aget state (int 2)))
           (aset state (int 1) (aget state (int 3)))
           (aset state (int 2) nil)
           (aset state (int 3) nil)
           r))))))

(defn entry [k v]
  #?(:clj (MapEntry. k v)
     :cljs (->MapEntry k v nil)))

(tests
  (sequence (comp (seq-diff :id) (map entry))
    [[{:id "alice" :email "alice@caramail.com"}]
     [{:id "alice" :email "alice@gmail.com"}
      {:id "bob" :name "bob@yahoo.com"}]
     [{:id "alice" :email "alice@gmail.com"}
      {:id "alice" :email "alice@msn.com"}
      {:id "bob" :name "bob@yahoo.com"}]
     [{:id "alice" :email "alice@gmail.com"}
      {:id "bob" :name "bob@yahoo.com"}
      {:id "alice" :email "alice@msn.com"}]
     [{:id "bob" :name "bob@yahoo.com"}
      {:id "alice" :email "alice@msn.com"}]
     []]) :=
  [[nil [?a nil]]
   [?a {:id "alice", :email "alice@caramail.com"}]
   [?a {:id "alice", :email "alice@gmail.com"}]
   [nil [?b nil]]
   [?b {:id "bob", :name "bob@yahoo.com"}]
   [nil [?c ?b]]
   [?c {:id "alice", :email "alice@msn.com"}]
   [nil [?b ?c]]
   [nil [?b ?a]]
   [?a {:id "alice", :email "alice@msn.com"}]
   [nil [?c ?c]]
   [nil [?a ?a]]
   [nil [?b ?b]]])

(defn insert-before [r x]
  (let [{:keys [index target]} r
        n (count index)]
    (case target
      nil (if (contains? index x)
            (assoc r :target x)
            (let [{:keys [keys vals]} r]
              (assoc r
                :vals (conj vals nil)
                :keys (conj keys x)
                :index (assoc index x n)
                :target x)))
      (let [t (index target)
            a (if (= x target) n (case x nil n (index x)))
            f (if (< t a) (dec a) a)
            s (compare f t)
            r (dissoc r :target)
            r (case s
                0 r
                (let [{:keys [vals keys]} r
                      v (nth vals t)]
                  (loop [vals vals, keys keys, index index, i t]
                    (let [j (+ i s)
                          k (nth keys j)
                          vals (assoc vals i (nth vals j))
                          keys (assoc keys i k)
                          index (assoc index k i)]
                      (if (== j f)
                        (assoc r
                          :vals (assoc vals j v)
                          :keys (assoc keys j target)
                          :index (assoc index target j))
                        (recur vals keys index j))))))]
        (if (= x target)
          (do (r/move (inc t))
              (-> r
                (update :vals pop)
                (update :keys pop)
                (update :index dissoc x)))
          (do (r/move (inc t) (inc f)) r))))))

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
  '{:vals [nil b c] :keys [:a :b :c] :index {:a 0, :b 1, :c 2}}
  )

(defn change [{:keys [index] :as r} k v]
  (if-some [i (index k)]
    (-> r
      (update :vals assoc i v)
      (update :failed
        (if (instance? Failure v)
          conj disj) i)) r))

(defn values [{:keys [vals failed]}]
  (if-some [[i] (seq failed)]
    (vals i) vals))

(defn seq-patch
  ([] {:vals [] :keys [] :index {} :failed (sorted-set)})
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
   (->> (m/ap (let [[id >x] (m/?> (->> >xs
                                    (m/eduction (seq-diff k) (map entry))
                                    (m/group-by key)))
                    >x-val (m/zip val >x)]
                (case id
                  nil (m/zip (partial hash-map id)
                        (m/relieve into >x-val))
                  (m/latest (partial hash-map id)
                    (f (m/relieve {} >x-val))))))
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
        @it := [{:id "bob" :email "BOB@YAHOO.COM"}])))
  )
