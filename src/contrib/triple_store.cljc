(ns contrib.triple-store
  (:refer-clojure :exclude [find])
  (:require [clojure.set :as set]
            [contrib.assert :as ca]
            [contrib.data :refer [->box]]))

;; ts - triple store
;; e  - entity    (id of entity)
;; a  - attribute (key of map)
;; v  - value     (val of map)
;; o  - options
;; nd - node, the entity map
;; ch - cache

;; [{:db/id 1, :foo 1, :bar 1}
;;  {:db/id 2, :foo 1, :bar 2}]
;; eav 1 :foo -> 1
;; ave :foo 1 -> (sorted-set 1 2) <- sorted so e.g. :parent e is well ordered
;; vea 1 1 -> #{:foo :bar}    CURRENTLY NOT USED/FILLED

(defrecord TripleStore [o eav ave])

(defn ->ts ([] (->ts {})) ([o] (->TripleStore o {} {})))

(defn add [ts nd]
  (let [e (get nd :db/id)
        -eav (->box (:eav ts)), -ave (->box (:ave ts))]
    (reduce-kv (fn [_ a v]
                 (-eav (update (-eav) e assoc a v))
                 (-ave (update (-ave) a update v (fnil conj (sorted-set)) e)))
      nil nd)
    (->TripleStore (:o ts) (-eav) (-ave))))

(defn del [ts e]
  (let [nd (-> ts :eav (get e))
        {:keys [o eav ave]} ts
        eav (dissoc eav e)
        ave (reduce-kv (fn [ave a v] (update ave a update v disj e)) ave nd)]
    (->TripleStore o eav ave)))

(defn upd [ts e a f]
  (let [v0 (-> ts :eav (get e) (get a))
        eav (update (:eav ts) e update a f)
        v1 (-> eav (get e) (get a))
        ave (if (= v0 v1)
              (:ave ts)
              (let [ave (update (:ave ts) a update v1 (fnil conj (sorted-set)) e)
                    ave (cond-> ave (contains? (get ave a) v0) (update a update v0 disj e))]
                (cond-> ave (not (seq (-> ave (get a) (get v0)))) (update a dissoc v0))))]
    (->TripleStore (:o ts) eav ave)))

(defn asc
  ([ts e a v] (upd ts e a (fn [_] v)))
  ([ts e a v & avs] (apply asc (asc ts e a v) e avs)))

;;;;;;;;;;;;;;;
;;; HELPERS ;;;
;;;;;;;;;;;;;;;

(defn ->node [ts e] (get (:eav ts) e))
(defn find [ts & kvs]
  (let [ret (reduce set/intersection (into [] (comp (partition-all 2) (map (fn [[k v]] (-> ts :ave (get k) (get v))))) kvs))]
    (when (seq ret) ret)))
(defn find1 [ts & kvs]
  (let [vs (apply find ts kvs)]
    (ca/check #(= 1 (count %)) vs)
    (first vs)))
