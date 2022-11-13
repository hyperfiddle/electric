(ns test.person-model
  (:require [contrib.data :refer [index-by]]))

(def schema
  (->> [{:db/ident :person/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one} :db/unique :db.unique/identity}
        {:db/ident :person/liked-tags, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/many}}
        {:db/ident :employee/manager, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
        {:db/ident :person/siblings, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}}
        {:db/ident :person/address, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/isComponent true}
        {:db/ident :person/summerHomes, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/isComponent true}
        {:db/ident :address/zip, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
        {:db/ident :person/age, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
        {:db/ident :person/bestFriend, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
        {:db/ident :person/friends, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}}]
       (index-by :db/ident)))

(def bob-map-stmt "covers all combinations"
  {:person/name "Bob" ; scalar one
   :person/address {:address/zip "12345"} ; ref one component
   :person/summerHomes [{:address/zip "11111"} ; ref many component
                        {:address/zip "22222"}]
   :person/liked-tags [:movies :ice-cream :clojure] ; scalar many
   :employee/manager {:person/name "Earnest"} ; ref one -> unique
   :person/siblings [{:person/name "Cindy"} ; ref many -> unique
                     {:person/name "David"}]
   :person/bestFriend "Benjamin"
   :person/friends ["Harry", "Yennefer"]})

(def bob-txn [bob-map-stmt
              [:db/cas 1 :person/age 41 42]
              [:user.fn/foo 'x 'y 'z 'q 'r]])

