(ns hyperfiddle.transaction-test
  (:require [datomic.client.api :as d]
            [hyperfiddle.rcf :as rcf]))

(defn ->client [] (d/client {:server-type :dev-local :storage-dir :mem :system "test"}))
(defn v! [db e a] (d/q '[:find ?v :in $ ?e ?a :where [?e ?a ?v]] (d/db db) e a))
(defn pull! [c e] (d/pull (d/db c) '[*] e))

(defmacro with-fresh-connection [sym & body]
  `(let [client# (->client)
         g# (d/create-database client# {:db-name "testing"})
         ~sym (d/connect client# {:db-name "testing"})]
     (do ~@body)))

(rcf/tests
  "a ref survives even if the entity it refers to gets retracted"
  (with-fresh-connection c
    (let [txs [[{:db/ident :aref :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/ident :bar :db/valueType :db.type/string :db/cardinality :db.cardinality/one}]
               [[:db/add "foo" :aref "-2"]
                [:db/add "-2" :bar "asdf"]]
               [[:db/retractEntity "-2"]]]
          ids (apply merge (map (comp :tempids #(d/transact c {:tx-data %})) txs))]
      (v! c (ids "foo") :aref) := [[_]])))
