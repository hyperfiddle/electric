(ns dustin.dev
  (:require
    [datomic.api :as d]))

(def ^:dynamic *$*)

(defn init []
  (d/create-database "datomic:mem://hello-world")
  (def conn (d/connect "datomic:mem://hello-world"))
  (def $ (-> (d/db conn)
           (d/with [{:db/ident :dustingetz/email :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                    {:db/ident :dustingetz/gender :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                    {:db/ident :dustingetz/shirt-size :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                    {:db/ident :dustingetz/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}])
           :db-after
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
           (d/with [{:dustingetz/email "alice@example.com" :dustingetz/gender :dustingetz/female :dustingetz/shirt-size :dustingetz/womens-large}
                    {:dustingetz/email "bob@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-large}
                    {:dustingetz/email "charlie@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-medium}])
           :db-after))
  (alter-var-root #'*$* (constantly $)))

(init)
