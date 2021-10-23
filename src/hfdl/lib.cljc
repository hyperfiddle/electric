(ns hfdl.lib
  (:require [hfdl.impl.gather :refer [gather]]
            [hfdl.impl.eventually :refer [eventually]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as str]))

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

(defn seq-diff [kf done]
  (fn [rf]
    (let [state (doto (object-array 4)
                  (aset (int 0) {})                         ;; prev {key [[id slot value]...]}
                  (aset (int 1) {})                         ;; curr
                  (aset (int 2) 0)                          ;; current slot
                  (aset (int 3) 0))                         ;; current id
          f (fn [d x]
              (let [p (aget state (int 0))
                    c (aget state (int 1))
                    i (aget state (int 2))
                    k (kf x)]
                (aset state (int 2) (inc i))
                (if-some [[[id j y] & m] (get p k)]
                  (let [d (if (= i j) d (assoc d [id :index] i))
                        d (if (= x y) d (assoc d [id :value] x))]
                    (aset state (int 0) (case m nil (dissoc p k) (assoc p k m)))
                    (aset state (int 1) (assoc c k (conj (get c k []) [id i x]))) d)
                  (let [id (aset state (int 3) (inc (aget state (int 3))))]
                    (aset state (int 1) (assoc c k (conj (get c k []) [id i x])))
                    (assoc d [id :index] i [id :value] x)))))
          g (fn [d _ m] (reduce (fn [d [id]] (assoc d [id :index] done [id :value] done)) d m))]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r xs]
         (let [d (reduce-kv g (reduce f {} xs) (aget state (int 0)))]
           (aset state (int 0) (aget state (int 1)))
           (aset state (int 1) {})
           (aset state (int 2) 0)
           (rf r d)))))))

(tests
  (sequence (seq-diff :id ::done)
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
      {:id "alice" :email "alice@msn.com"}]]) :=
  [{[1 :index] 0,
    [1 :value] {:id "alice", :email "alice@caramail.com"}}
   {[1 :value] {:id "alice", :email "alice@gmail.com"},
    [2 :index] 1,
    [2 :value] {:id "bob", :name "bob@yahoo.com"}}
   {[3 :index] 1,
    [3 :value] {:id "alice", :email "alice@msn.com"},
    [2 :index] 2}
   {[2 :index] 1,
    [3 :index] 2}
   {[2 :index] 0,
    [1 :index] 1,
    [1 :value] {:id "alice", :email "alice@msn.com"},
    [3 :index] ::done,
    [3 :value] ::done}])

(defn conj-nil [r] (conj r nil))

(defn grow [r n]
  (nth (iterate conj-nil r) n))

(defn shrink [r n]
  (nth (iterate pop r) n))

(defn resize [r n]
  (if (neg? n)
    (shrink r (- n))
    (if (pos? n)
      (grow r n) r)))

(def merge-diff (partial mapv merge))
(def empty-diff [{} {}])
(defn seq-patch
  ([] [[] {}])
  ([x] x)
  ([[v s] [id->slot id->value]]
   (let [slots (reduce-kv
                 (fn [m id slot]
                   (case slot
                     -1 (dissoc m id)
                     (assoc m id slot)))
                 s id->slot)
         values (reduce-kv
                  (fn [values id value]
                    (assoc values (get slots id) value))
                  (reduce-kv
                    (fn [values id slot]
                      (case slot
                        -1 values
                        (assoc values slot (get v (get s id)))))
                    (resize v (- (count slots) (count s))) id->slot) id->value)]
     [values slots])))

(tests
  (reduce seq-patch (seq-patch)
    [[{1 0} {1 {:id "alice", :email "alice@caramail.com"}}]
     [{2 1} {1 {:id "alice", :email "alice@gmail.com"}
             2 {:id "bob", :name "bob@yahoo.com"}}]
     [{3 1 2 2} {3 {:id "alice", :email "alice@msn.com"}}]
     [{2 1 3 2} {}]
     [{2 0 1 1 3 -1} {1 {:id "alice", :email "alice@msn.com"}}]])
  :=
  [[{:id "bob", :name "bob@yahoo.com"}
    {:id "alice", :email "alice@msn.com"}]
   {1 1, 2 0}])

(defn map-by "
Given a function and a continuous flow of collections, returns a continuous flow of vectors of the same size as input
collection, where values are produced by the continuous flow returned by the function when called with the continuous
flow of values matching the identity provided by key function, defaulting to identity."
  ([f >xs] (map-by identity f >xs))
  ([k f >xs]
   (->> >xs
     (m/eduction (seq-diff k ::done) cat)
     (m/group-by key)
     (m/eduction
       (map (fn [[[id tag] >x]]
              (case tag
                :index (->> >x
                         (m/eduction (map val)
                           (take-while (complement #{::done})))
                         (m/relieve {})
                         (eventually -1)
                         (m/latest (partial update empty-diff 0 assoc id)))
                :value (->> >x
                         (m/eduction (map val)
                           (take-while (complement #{::done})))
                         (m/relieve {})
                         (f)
                         (m/latest (partial update empty-diff 1 assoc id)))))))
     (gather merge-diff)
     (m/reductions seq-patch)
     (m/latest #(get % 0)))))

(tests
  (let [!xs (atom [])
        it ((map-by :id
              (partial m/latest #(update % :email str/upper-case))
              (m/watch !xs))
            #() #())]
    @it := []
    (swap! !xs conj {:id "alice" :email "alice@caramail.com"})
    @it := [{:id "alice" :email "ALICE@CARAMAIL.COM"}]
    (swap! !xs assoc 0 {:id "alice" :email "alice@gmail.com"})
    @it := [{:id "alice" :email "ALICE@GMAIL.COM"}]
    (swap! !xs conj {:id "bob" :email "bob@yahoo.com"})
    (let [x @it]
      x := [{:id "alice" :email "ALICE@GMAIL.COM"}
            {:id "alice" :email "BOB@YAHOO.COM"}]
      (swap! !xs reverse)
      (let [y @it]
        (identical? (get x 0) (get y 1)) := true
        (identical? (get x 1) (get y 0)) := true)))
  )