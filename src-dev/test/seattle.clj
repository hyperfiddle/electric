(ns test.seattle
  (:require [contrib.data :refer [index-by]]))

(def schema
  (->> [{:db/ident :community/name, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/fulltext true, :db/doc "A community's name"}
        {:db/ident :community/url, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "A community's url"}
        {:db/ident :community/neighborhood, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/doc "A community's neighborhood"}
        {:db/ident :community/category, :db/valueType :db.type/string, :db/cardinality :db.cardinality/many, :db/fulltext true, :db/doc "All community categories"}
        {:db/ident :community/orgtype, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/doc "A community orgtype enum value"}
        {:db/ident :community/type, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/doc "Community type enum values"}
        {:db/ident :community/board :db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/doc "People who sit on the community board"}
        {:db/ident :person/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/doc "A person's full name"}
        {:db/ident :neighborhood/name, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity, :db/doc "A unique neighborhood name (upsertable)"}
        {:db/ident :neighborhood/district, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/doc "A neighborhood's district"}
        {:db/ident :district/name, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity, :db/doc "A unique district name (upsertable)"}
        {:db/ident :district/region, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/doc "A district region enum value"}]
       (index-by :db/ident)))