(ns contrib.datomic-peer-contrib
  (:require [contrib.data :refer [index-by unqualify]]
            contrib.datomic-common-contrib
            [contrib.datomic-peer-m :as d]
            [hyperfiddle.photon :as p] ; ?
            [hyperfiddle.rcf :refer [tests % tap]]
            [missionary.core :as m]))

(def identify contrib.datomic-common-contrib/identify)
(def identities contrib.datomic-common-contrib/identities)
(def reverse-attr contrib.datomic-common-contrib/reverse-attr)

(defn attributes>
  ([db] (attributes> db [:db/ident]))
  ([db pull-pattern]
   (->> (d/qseq {:query '[:find (pull ?e pattern)
                          :in $ pattern
                          :where [?e :db/valueType _]]
                 :args [db pull-pattern]})
        (m/eduction (map first)))))

(tests
  (m/? (m/reduce conj [] (attributes> user/datomic-db [:db/ident])))
  (m/? (m/reduce conj [] (attributes> user/datomic-db)))
  (m/? (d/pull user/datomic-db {:eid 50 :selector [:db/ident :db/valueType :db/cardinality]})))

(defn schema! [db] ; todo stream
  (m/sp
    (let [>as (attributes> db [:db/ident
                               {:db/valueType [:db/ident]}
                               {:db/cardinality [:db/ident]}])

          ; :db/id is a renderable attr in the semantic UI, schema metadata describes how
          as (cons {:db/ident :db/id
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/long}
                    #_#_:db/unique :db.unique/identity}
                   (m/? (m/reduce conj [] >as)))]

      ; todo - streaming group-by should be faster – shouldn't need to wait for all attrs to load
      ; todo - only load the attrs that the renderers actually needs
      (index-by :db/ident as, :compare compare))))

(defn schema> [db] (p/task->cp (schema! db)))

(tests
  (:db/ident (m/? (schema! user/datomic-db)))
  := #:db{:ident :db/ident,
          :valueType #:db{:ident :db.type/keyword},
          :cardinality #:db{:ident :db.cardinality/one}}

  (:db/id (m/? (schema! user/datomic-db)))
  := #:db{:ident :db/id,
          :cardinality #:db{:ident :db.cardinality/one},
          :valueType #:db{:ident :db.type/long}}

  (count (m/? (schema! user/datomic-db))))
