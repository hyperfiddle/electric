(ns leo.datomic
  (:require [missionary.core :as m]
            [clojure.core.async :as a]
            [datomic.client.api.async :as d])
  (:import (missionary Cancelled)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn chan-read [chan]
  (fn [success failure]
    (let [cancel (a/chan)]
      (a/go (let [[v port] (a/alts! [chan cancel])]
              (if (= port cancel)
                (failure (Cancelled.))
                (if (:cognitect.anomalies/category v)
                  (failure (ex-info (or (:cognitect.anomalies/message v)
                                      (-> v :datomic.client/http-error-body :cause)
                                      "Datomic Client Exception") v))
                  (success v)))))
      #(a/close! cancel))))

(defn qs [arg-map]
  (m/ap
    (let [c (d/qseq arg-map)]
      (loop []
        (if-some [chunk (m/? (chan-read c))]
          (m/amb (m/?> (m/seed chunk)) (recur)) (m/amb))))))

(defn partition-size
  "A task computing the count of artists with name between given bounds (exclusive)."
  [args ln rn]
  (m/reduce (comp reduced first {}) 0
    (qs {:query {:find  ['(count ?e)]
                 :where (-> [['?e :artist/name '?n]]
                          (into (when ln [[(list '< ln '?n)]]))
                          (into (when rn [[(list '> rn '?n)]])))}
         :args  args})))

(defn choose-pivot
  "A task choosing an arbitrary artist name between given bounds (exclusive)."
  [args ln rn]
  (m/reduce (comp reduced first {}) nil
    (qs {:limit 1
         :query {:find  ['?n]
                 :where (-> [['?e :artist/name '?n]]
                          (into (when ln [[(list '< ln '?n)]]))
                          (into (when rn [[(list '> rn '?n)]])))}
         :args  args})))

(defn find-kth-artist-name
  "A task returning the k-th artist name using quickselect algorithm - https://en.wikipedia.org/wiki/Quickselect"
  [args ^long k]
  (assert (not (neg? k)))
  (m/sp
    (let [n (long (m/? (partition-size args nil nil)))]
      (when (< k n)
        (loop [k k, n n, l nil, r nil]
          (let [p (m/? (choose-pivot args l r))
                s (long (m/? (partition-size args l p)))]
            (if (< k s)
              (recur k s l p)
              (if (< s k)
                (let [s (unchecked-inc s)]
                  (recur (- k s) (- n s) p r))
                p))))))))

(defn heap-enqueue!
  "Insert a value in a heap."
  [^objects heap x]
  (let [n (unchecked-inc (long (aget heap 0)))]
    (aset heap 0 (identity n))
    (aset heap n x)
    (loop [i n]
      (when-not (== 1 i)
        (let [p (bit-shift-right i 1)
              x (aget heap i)
              y (aget heap p)]
          (when-not (neg? (compare x y))
            (aset heap p x)
            (aset heap i y)
            (recur p)))))))

(defn heap-dequeue!
  "Remove and returns the largest value of a heap."
  [^objects heap]
  (let [n (long (aget heap 0))
        x (aget heap 1)]
    (aset heap 0 (identity (unchecked-dec n)))
    (aset heap 1 (aget heap n))
    (aset heap n nil)
    (loop [i 1]
      (let [l (bit-shift-left i 1)]
        (when (< l n)
          (let [x (aget heap i)
                y (aget heap l)
                r (unchecked-inc l)]
            (if (< r n)
              (let [z (aget heap r)]
                (if (neg? (compare y z))
                  (when (neg? (compare x z))
                    (aset heap r x)
                    (aset heap i z)
                    (recur r))
                  (when (neg? (compare x y))
                    (aset heap l x)
                    (aset heap i y)
                    (recur l))))
              (when (neg? (compare x y))
                (aset heap l x)
                (aset heap i y)
                (recur l))))))) x))

(defn heap-create
  "Returns a fresh max-heap with given capacity."
  [^long cap]
  (doto (object-array (unchecked-inc cap))
    (aset 0 (identity 0))))                                 ;; force boxing

(defn heap-flush!
  "Flush a heap and return its contents as an ordered sequence."
  [^objects heap]
  (loop [xs ()]
    (if (zero? (long (aget heap 0)))
      xs (recur (conj xs (heap-dequeue! heap))))))

(defn heap-replace!
  "Insert a value in a heap, then remove the largest value if heap size reached capacity."
  [^objects heap x]
  (heap-enqueue! heap x)
  (when (== (long (aget heap 0)) (unchecked-dec-int (alength heap)))
    (heap-dequeue! heap)) heap)

(comment
  "Find 10 smallest numbers - https://en.wikipedia.org/wiki/Partial_sorting#Heap-based_solution"
  (def data-10M (shuffle (range 10000000)))

  ;; naive solution
  (time (take 10 (into (sorted-set) data-10M)))
  := (range 10)

  (time (take 10 (sort data-10M)))
  (time (take 10 (doall (sort data-10M))))

  ;; store only the 10 smallest
  (time (reverse
          (reduce (fn [s x]
                    (let [s (conj s x)]
                      (if (< 10 (count s))
                        (disj s (first s)) s)))
            (sorted-set-by (fn [x y] (compare y x)))
            data-10M)))
  := (range 10)

  ;; further improvement - mutable heap
  (time (heap-flush!
          (reduce heap-replace!
            (heap-create (inc 10)) data-10M)))
  := (range 10)


  )

(defn find-smallest-artist-names
  "A task returning an ordered sequence of artist names larger than or equal to `start` and bounded by `limit`."
  [args start ^long limit]
  (assert (some? start))
  (assert (pos? limit))
  (m/sp
    (cons start
      (when (< 1 limit)
        (heap-flush!
          (m/?
            (->> (qs {:query {:find  ['?n]
                              :where [['?e :artist/name '?n]
                                      [(list '< start '?n)]]}
                      :args  args})
              (m/eduction (map first))
              (m/reduce heap-replace! (heap-create limit)))))))))

(defn find-paginated-artist-names
  "A task returning an ordered sequence of artist names starting with the `offset`-th and bounded by `limit`."
  [args ^long offset ^long limit]
  (assert (not (neg? offset)))
  (assert (pos? limit))
  (m/sp (when-some [start (m/? (find-kth-artist-name args offset))]
          (m/? (find-smallest-artist-names args start limit)))))

(comment
  (require '[leo.entrypoint :as user])

  (m/? (find-kth-artist-name [user/db] 0)) := "\"Brother\" Jack McDuff"
  (m/? (find-kth-artist-name [user/db] 100)) := "Alice Cooper"
  (m/? (find-kth-artist-name [user/db] 200)) := "April Wine"
  (m/? (find-kth-artist-name [user/db] 1000)) := "Don Cherry"
  (m/? (find-kth-artist-name [user/db] 4000)) := "The Pentangle"
  (m/? (find-kth-artist-name [user/db] 5000)) := nil

  (time (m/? (find-smallest-artist-names [user/db] "Don Cherry" 10)))
  := ["Don Cherry"
      "Don Ciccone"
      "Don Covay & The Jefferson Lemon Blues Band"
      "Don Elliott"
      "Don Ellis"
      "Don Fardon"
      "Don Harper"
      "Don McHan"
      "Don McLean"
      "Don Nix"]

  (time (m/? (find-paginated-artist-names [user/db] 1000 10)))
  := ["Don Cherry"
      "Don Ciccone"
      "Don Covay & The Jefferson Lemon Blues Band"
      "Don Elliott"
      "Don Ellis"
      "Don Fardon"
      "Don Harper"
      "Don McHan"
      "Don McLean"
      "Don Nix"]

  (m/? (find-paginated-artist-names [user/db] 5000 10)) := nil

  )

(comment

  (time
    (->> (m/? (m/reduce conj (qs {:query '[:find ?name ?title ?album ?year
                                          :in $
                                          :where
                                          [?a :artist/name ?name]
                                          [?t :track/artists ?a]
                                          [?t :track/name ?title]
                                          [?m :medium/tracks ?t]
                                          [?r :release/media ?m]
                                          [?r :release/name ?album]
                                          [?r :release/year ?year]
                                          [(<= "John Lennon" ?name)]]
                                 :args  [user/db]})))
      (sort)
      (take 10)))

  )