(ns geoffrey.diff
  (:require [datascript.core :as d]
            [missionary.core :as m]
            [clojure.set :as set]
            [minitest :refer [tests]]))

(defn fixtures [$]
  (-> $
      (d/with [{:dustingetz/type :dustingetz/gender :db/ident :dustingetz/male}
               {:dustingetz/type :dustingetz/gender :db/ident :dustingetz/female}])
      :db-after
      (d/with [{:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-small :dustingetz/gender :dustingetz/male}
               {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-medium :dustingetz/gender :dustingetz/male}
               {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-large :dustingetz/gender :dustingetz/male}
               {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-small :dustingetz/gender :dustingetz/female}
               {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-medium :dustingetz/gender :dustingetz/female}
               {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-large :dustingetz/gender :dustingetz/female}])
      :db-after
      (d/with [{:dustingetz/email "alice@example.com" :dustingetz/gender :dustingetz/female :dustingetz/shirt-size :dustingetz/womens-large
                :dustingetz/tags  [:a :b :c]}
               {:dustingetz/email "bob@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-large
                :dustingetz/tags  [:b]}
               {:dustingetz/email "charlie@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-medium}])
      :db-after
      (d/with [{:dustingetz/email "alice@example.com" :dustingetz/gender :dustingetz/female :dustingetz/shirt-size :dustingetz/womens-large}
               {:dustingetz/email "bob@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-large}
               {:dustingetz/email "charlie@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-medium}])

      :db-after))

(def ^:dynamic *$*)

(defn init-datascript []
  (def schema
    {:dustingetz/email      {#_#_:db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
     :dustingetz/gender     {#_#_:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
     :dustingetz/shirt-size {#_#_:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
     :dustingetz/type       {#_#_:db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
     :dustingetz/tags       {#_#_:db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}
     :db/ident              {:db/unique :db.unique/identity}})
  (let [$ (-> (d/create-conn schema) d/db fixtures)]
    (alter-var-root #'*$* (constantly $))
    :ok))

(defn diff [- init]
  (fn [rf]
    (let [p (volatile! init)]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r x]
         (let [r (rf r (- x @p))]
           (vreset! p x) r))))))

(defn diff-set [>xs]
  (m/transform (diff (fn [x y]
                       (let [x (set x)
                             y (set y)]
                         [(set/difference x y)
                          (set/difference y x)]))
                     #{}) >xs))

(defn entity-ks' [>db eid]
  (->> (m/latest (fn [db] (keys (d/entity db eid))) >db)
       (diff-set)))

(defn q' [>query & >inputs]
  (->> (apply m/latest d/q >query >inputs)
       (diff-set)))

(tests
 (init-datascript) := :ok)

(comment
  (d/q '[:find [?e ...] :where [?e :dustingetz/email]] *$*))

(tests
 (def !db (atom *$*)) (def >db (m/watch !db))
 (def !needle (atom "alice@example.com")) (def >needle (m/watch !needle))
 (def !q (atom '[:in $ ?needle
                 :find [?e ...]
                 :where [?e :dustingetz/email ?needle]]))
 (def >q (m/watch !q))

 (def qq (q' >q >db >needle))
 (def it (qq #(prn :ready) #(prn :done)))
 @it := [#{9} #{}]

 (reset! !needle "bob@example.com")

 @it := [#{10} #{9}]
 )

(comment
  (tests
   (do
     (def !db (atom *$*)) (def >db (m/watch !db))

     (def qq (entity-ks' >db 9))
     (def it (qq #(prn :ready) #(prn :done))))
   @it := [#{:dustingetz/gender :dustingetz/email :dustingetz/shirt-size :dustingetz/tags} #{}]

   (reset! !db (:db-after (d/with *$* [[:db/add 9 :tests/extra-attr true]])))

   @it := [#{:tests/extra-attr} #{}]

   (reset! !db (:db-after (d/with *$* [[:db/retract 9 :tests/extra-attr true]])))

   @it := [#{} #{:tests/extra-attr}]
   ))

(defn cardinality [db attr]
  (get-in db [:schema attr :db/cardinality]))

(def xf-diff-value
  (fn [rf]
    (let [p (volatile! #{})]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r [db k v]]
         (let [card        (cardinality db k)
               [adds rets] (case card
                             :db.cardinality/one  [#{v} @p]
                             :db.cardinality/many (let [x (set v)
                                                        y @p]
                                                    [(set/difference x y)
                                                     (set/difference y x)]))
               r           (rf r [adds rets])]
               (vreset! p (case card
                            :db.cardinality/one  #{v}
                            :db.cardinality/many v))
               r))))))

(defn entity-get' [>db eid k]
  (->> (m/latest (fn [db] [db k (k (d/entity db eid))]) >db)
       (m/transform xf-diff-value)))

(defn patch-set
  "Given a diff produced by `diff-map`, patch the given map."
  ([] (patch-set #{}))
  ([s] (let [prev (volatile! s)]
         (fn [[adds rets]]
           (let [next (reduce disj (into @prev adds) rets)]
             (vreset! prev next)
             next)))))

(defn patch-value
  [[adds _]]
  (first adds))

(comment
  (do
    (def !db (atom *$*)) (def >db (m/watch !db))

    (def qq (entity-get' >db 9 :dustingetz/gender))
    (def it (qq #(prn :ready) #(prn :done)))
    )

  @it := [#{:dustingetz/female} #{}]

  (reset! !db (:db-after (d/with *$* [[:db/add 9 :dustingetz/gender :dustingetz/male]])))

  @it := [#{:dustingetz/male} #{:dustingetz/female}]

  (reset! !db (:db-after (d/with *$* [[:db/retract 9 :dustingetz/gender]])))

  @it := [#{nil} #{:dustingetz/male}]
  )

(comment
  (do
    (def !db (atom *$*)) (def >db (m/watch !db))
    (def qq (entity-get' >db 9 :dustingetz/tags))
    (def it (qq #(prn :ready) #(prn :done)))
    )

  @it := [#{:a :b :c} #{}]

  (reset! !db (:db-after (d/with *$* [[:db/add 9 :dustingetz/tags :d]])))

  @it := [#{:d} #{}]

  (reset! !db (:db-after (d/with *$* [[:db/retract 9 :dustingetz/tags :d]])))

  @it := [#{} #{:d}]

  )

(comment
  (do
    (def !db (atom *$*)) (def >db (m/watch !db))

    (def qq (->> (entity-get' >db 9 :dustingetz/gender)
                 (m/latest patch-value)))
    (def it (qq #(prn :ready) #(prn :done)))
    )

  @it := :dustingetz/female

  (reset! !db (:db-after (d/with *$* [[:db/add 9 :dustingetz/gender :dustingetz/male]])))

  @it := :dustingetz/male

  (reset! !db (:db-after (d/with *$* [[:db/retract 9 :dustingetz/gender]])))

  @it := nil
  )

(comment
  (do
    (def !db (atom *$*)) (def >db (m/watch !db))
    (def qq (m/latest (patch-set) (entity-get' >db 9 :dustingetz/tags)))
    (def it (qq #(prn :ready) #(prn :done)))
    )

  @it := #{:a :b :c}

  (reset! !db (:db-after (d/with *$* [[:db/add 9 :dustingetz/tags :d]])))

  @it := #{:a :b :c :d}

  (reset! !db (:db-after (d/with *$* [[:db/retract 9 :dustingetz/tags :d]])))

  @it := #{:a :b :c}
  )

;; ¹ (UnsupportedOperationException) at datascript.impl.entity.Entity/empty
;; (entity.cljc:47)
