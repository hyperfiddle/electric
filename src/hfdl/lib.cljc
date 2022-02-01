(ns hfdl.lib
  (:require [hfdl.impl.gather :refer [gather]]
            [hfdl.impl.runtime]
            [hfdl.impl.eventually :refer [eventually]]
            [hfdl.impl.runtime :as r]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as str])
  (:import missionary.Cancelled))

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

(defn seq-diff [kf done]
  (fn [rf]
    (let [state (doto (object-array 5)
                  (aset (int 0) {})                     ;; prev {key [[id slot value]...]}
                  (aset (int 1) {})                     ;; curr
                  (aset (int 2) 0)                      ;; current slot
                  (aset (int 3) 0))                     ;; current id
          scan (partial reduce
                 (fn [r x]
                   (let [p (aget state (int 0))
                         c (aget state (int 1))
                         i (aget state (int 2))
                         k (kf x)]
                     (aset state (int 2) (inc i))
                     (if-some [[[id j y] & m] (get p k)]
                       (let [r (if (= i j) r (update r :move assoc id i))
                             r (if (= x y) r (update r :change assoc id x))]
                         (aset state (int 0) (case m nil (dissoc p k) (assoc p k m)))
                         (aset state (int 1) (assoc c k (conj (get c k []) [id i x]))) r)
                       (let [id (aset state (int 3) (inc (aget state (int 3))))]
                         (aset state (int 1) (assoc c k (conj (get c k []) [id i x])))
                         (update r :add assoc id [i x]))))) {})
          send-remove (partial reduce (fn [r id] (-> r (rf [id :index done]) (rf [id :value done]))))
          send-move   (partial reduce-kv (fn [r id i] (rf r [id :index i])))
          send-change (partial reduce-kv (fn [r id x] (rf r [id :value x])))
          send-add    (partial reduce-kv (fn [r id [i x]] (-> r (rf [id :index i]) (rf [id :value x]))))]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r xs]
         (let [failure (r/failure xs)
               {:keys [add move change]} (when-not failure (scan xs))
               change (merge change (when-not (= failure (aget state (int 4))) {0 failure}))
               remove (val-first (aget state (int 0)))]
           (aset state (int 0) (aget state (int 1)))
           (aset state (int 1) {})
           (aset state (int 2) 0)
           (aset state (int 4) failure)
           (-> r
             (send-remove remove)
             (send-move move)
             (send-change change)
             (send-add add))))))))

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
      {:id "alice" :email "alice@msn.com"}]
     (r/->Failure nil)]) :=
  [[1 :index 0]
   [1 :value {:id "alice", :email "alice@caramail.com"}]
   [1 :value {:id "alice", :email "alice@gmail.com"}]
   [2 :index 1]
   [2 :value {:id "bob", :name "bob@yahoo.com"}]
   [2 :index 2]
   [3 :index 1]
   [3 :value {:id "alice", :email "alice@msn.com"}]
   [2 :index 1]
   [3 :index 2]
   [3 :index ::done]
   [3 :value ::done]
   [2 :index 0]
   [1 :index 1]
   [1 :value {:id "alice", :email "alice@msn.com"}]
   [2 :index :hfdl.lib/done]
   [2 :value :hfdl.lib/done]
   [1 :index :hfdl.lib/done]
   [1 :value :hfdl.lib/done]
   [0 :value (r/->Failure nil)]])

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
  ([[v s _] [id->slot id->value]]
   (let [slots (reduce-kv
                 (fn [m id slot]
                   (case slot
                     -1 (dissoc m id)
                     (assoc m id slot)))
                 s id->slot)
         values (reduce-kv
                  (fn [values id value]
                    (if-some [slot (get slots id)]
                      (assoc values slot value) values))
                  (reduce-kv
                    (fn [values id slot]
                      (case slot
                        -1 values
                        (assoc values slot (get v (get s id)))))
                    (resize v (- (count slots) (count s))) id->slot)
                  (dissoc id->value 0))]
     [values slots (id->value 0)])))

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
   {1 1, 2 0} nil])

(defn map-by "
Given a function and a continuous flow of collections, returns a continuous flow of vectors of the same size as input
collection, where values are produced by the continuous flow returned by the function when called with the continuous
flow of values matching the identity provided by key function, defaulting to identity."
  ([f >xs] (map-by identity f >xs))
  ([k f >xs] (map-by nil k f >xs))
  ([e k f >xs]
   (->> >xs
     (m/eduction (seq-diff k ::done))
     (m/group-by pop)
     (m/eduction
       (map (fn [[[id tag] >x]]
              (case id
                0 (->> >x
                    (m/eduction (map peek))
                    (m/relieve {})
                    (m/latest (partial update empty-diff 1 assoc id)))
                (case tag
                  :index (->> >x
                           (m/eduction (map peek)
                             (take-while (complement #{::done}))
                             (append -1))
                           (m/relieve {})
                           (m/latest (partial update empty-diff 0 assoc id)))
                  :value (->> >x
                           (m/eduction (map peek)
                             (take-while (complement #{::done}))
                             (append e))
                           (m/relieve {})
                           (f) (m/latest (partial update empty-diff 1 assoc id))))))))
     (gather merge-diff)
     (m/reductions seq-patch)
     (m/latest (fn [[v _ f]]
                 (if (r/failure f)
                   f (if-some [f (apply r/failure v)]
                       f v)))))))

(tests
  (let [!xs (atom [])
        it ((map-by :id
              (partial m/latest (fn [x] (when (some? x) (update x :email str/upper-case))))
              (m/watch !xs)) #() #())]
    @it := []
    (swap! !xs conj {:id "alice" :email "alice@caramail.com"})
    @it := [{:id "alice" :email "ALICE@CARAMAIL.COM"}]
    (swap! !xs assoc 0 {:id "alice" :email "alice@gmail.com"})
    @it := [{:id "alice" :email "ALICE@GMAIL.COM"}]
    (swap! !xs conj {:id "bob" :email "bob@yahoo.com"})
    (let [x @it]
      x := [{:id "alice" :email "ALICE@GMAIL.COM"}
            {:id "alice" :email "BOB@YAHOO.COM"}]
      (swap! !xs (comp vec reverse))
      (let [y @it]
        (identical? (get x 0) (get y 1)) := true
        (identical? (get x 1) (get y 0)) := true
        (swap! !xs pop)
        @it := [{:id "alice" :email "BOB@YAHOO.COM"}]))))

;; (tests
;;  ;; (require '[hfdl.impl.runtime])
;;  (let [!xs (atom [])
;;        failure (hfdl.impl.runtime/->Failure ":trollface:")
;;        it ((map-by identity
;;                    (partial m/latest identity)
;;                    (m/watch !xs))
;;            #() #())]
;;    @it := []
;;    (reset! !xs failure)
;;    @it := failure
;;    ))
