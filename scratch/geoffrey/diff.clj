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
                       [(set/difference x y)
                        (set/difference y x)])
                     #{}) >xs))

(defn entity-get [>db >eid >k]
  (m/latest (fn [db eid k] (k (d/entity db eid))) >db >eid >k))

(defn entity-ks [>db >eid]
  (m/latest (fn [db eid] (set (keys (d/entity db eid)))) >db >eid))

(defn q' [>query >db & >inputs]
  (->> (apply m/latest (fn [query & inputs] (set (apply d/q query inputs))) >query >db >inputs)
       (diff-set)))

(def xf-entities
  (fn [rf]
    (let [p (volatile! {})]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r [db [adds rets]]]
         (let [p' (as-> @p p
                    (reduce dissoc p rets)
                    (reduce (fn [r eid]
                              (assoc r eid (d/entity db eid)))
                            p
                            adds))
               r  (rf r (set (vals p')))]
           (vreset! p p')
           r))))))

(defn entities [>db >q'-diff]
  (->> (m/latest vector >db >q'-diff)
       (m/transform xf-entities)))

(tests
 (init-datascript) := :ok)

(comment
  (d/q '[:find [?e ...] :where [?e]] *$*))

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
 (do
   (def !db (atom *$*)) (def >db (m/watch !db))
   (def !needle (atom "alice@example.com")) (def >needle (m/watch !needle))
   (def !q (atom '[:in $ ?needle
                   :find [?e ...]
                   :where [?e :dustingetz/email ?needle]]))
   (def >q (m/watch !q))

   (def qq (entities >db (q' >q >db >needle)))
   (def it (qq #(prn :ready) #(prn :done)))
   (def entities @it))
 entities := #{#:db{:id 9}} ;; Break minitest¹ (:dustingetz/email (first entities)) := "alice@example.com"

 (reset! !needle "bob@example.com")
 (def entities @it)
 entities := #{#:db{:id 10}}
 )


(comment
  (do
    (def !db (atom *$*)) (def >db (m/watch !db))
    (def !needle (atom "alice@example.com")) (def >needle (m/watch !needle))
    (def !q (atom '[:in $ ?needle
                    :find [?e ...]
                    :where [?e :dustingetz/email ?needle]]))
    (def >q (m/watch !q))

    (def qq (->> (entities >db (q' >q >db >needle))
                 (m/latest first)
                 (m/latest :db/id)
                 (entity-ks >db)))
    (def it (qq #(prn :ready) #(prn :done)))
    (def entities @it))
  entities := #{:dustingetz/gender :dustingetz/email :dustingetz/shirt-size :dustingetz/tags}

  (reset! !db (:db-after (d/with *$* [{:dustingetz/email "alice@example.com"
                                       :tests/extra-attr true}])))

  @it := #{:dustingetz/gender :dustingetz/email :dustingetz/shirt-size :dustingetz/tags :tests/extra-attr}
  )


;; ¹ (UnsupportedOperationException) at datascript.impl.entity.Entity/empty
;; (entity.cljc:47)
