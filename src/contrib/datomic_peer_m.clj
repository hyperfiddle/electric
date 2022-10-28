(ns contrib.datomic-peer-m
  (:require [clojure.core.protocols :as ccp :refer [nav]]
            [clojure.datafy :refer [datafy]]
            [contrib.data :refer [omit-keys-ns auto-props]]
            [contrib.missionary-contrib :as mx]
            [datomic.api :as d]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests tap %]])
  (:import (datomic.db Datum)))

(defn tempid? [x] (or (string? x) (neg? x)))

(tests
  (tempid? -1) := true
  (tempid? 1) := false
  (tempid? 0) := false
  (tempid? "a") := true)

(defn connect [uri] (d/connect uri))

(defn db [conn] (m/via m/blk (d/db conn)))

(def db-stats
  (let [x (try @(requiring-resolve 'datomic.api/db-stats) ; since 1.0.6344
               ; Cannot invoke "java.util.concurrent.Future.get()" because "fut" is null
               (catch NullPointerException e
                 (ex-info "datomic.api/db-stats not available, check Datomic version >= 1.0.6344" {})))]
    (fn db-stats [db]
      (m/sp (if (instance? Exception x)
              (throw x) (x db))))))

(comment (m/? (db-stats test/datomic-db)))

(defn with [db tx-data] (m/via m/blk (d/with db tx-data)))

(def with-db with)

(extend-protocol ccp/Datafiable
  Datum
  (datafy [^Datum [e a v tx op]] [e a v tx op]))

; Fix Datomic handling of string tempids - https://github.com/hyperfiddle/hyperfiddle-2020/issues/584

(defn entity [db e]
  (if (tempid? e)
    (m/sp {:db/id e})
    (m/via m/blk (d/entity db e))))

(defn touch [e] (m/via m/blk (d/touch e)))

(defn pull
  ([db {:keys [selector eid]}] (pull db selector eid))
  ([db pattern eid]
   (if (tempid? eid)
     (m/sp (if (some #{:db/id} pattern) {:db/id eid} {}))
     (m/via m/blk (d/pull db pattern eid)))))

(tests
  "control - datomic operators work on number tempids"
  (pr-str (d/entity test/datomic-db -1)) := (pr-str {:db/id -1}) ; :db/id is virtual key so test print repr
  (d/pull test/datomic-db [:db/id] -1) := #:db{:id -1}
  (d/pull test/datomic-db ['*] -1) := #:db{:id -1}

  "control - datomic operators crash on string tempids, wtf"
  (d/entity test/datomic-db "tempid-1") :throws datomic.impl.Exceptions$IllegalArgumentExceptionInfo
  (d/pull test/datomic-db [:db/id] "a") :throws datomic.impl.Exceptions$IllegalArgumentExceptionInfo
  (d/pull test/datomic-db ['*] "a") :throws datomic.impl.Exceptions$IllegalArgumentExceptionInfo

  "hyperfiddle needs this defined to represent empty forms"
  (m/? (entity test/datomic-db "tempid-1")) := #:db{:id "tempid-1"}
  (m/? (pull test/datomic-db [:db/id] "a")) := {:db/id "a"}
  (m/? (pull test/datomic-db [:db/ident] "a")) := {})

(defn pull-sorted
  ([db {:keys [selector eid] :as arg-map}] (pull-sorted db selector eid (dissoc arg-map :selector :eid)))
  ([db pattern eid & [arg-map]]
   (let [{:keys [::compare]} (auto-props arg-map)]
     (m/sp (let [tree (m/? (pull db pattern eid))]
             (if compare
               ; Use datafy/nav to sort on the fly? or pre-sort here?
               ; need to know cardinality many attrs and sort on nav
               ; unless we can use datoms API to make datomic sort them
               (m/? (m/via m/blk (into (sorted-map-by compare) tree)))
               tree))))))

(tests
  "pulls are sorted at top layer"
  (take 3 (keys (m/? (pull-sorted test/datomic-db {:eid 50 :selector '[*] ::compare compare}))))
  := [:db/cardinality :db/doc :db/id] ; sorted!

  "pulls are sorted at intermedate layers"
  'todo)

(defn datoms>
  ([db {:keys [index components]}] (apply datoms> db index components))
  ([db index & components]
   (mx/seq-consumer (apply d/datoms db index components))))

(tests
  "control"
  (take 3 (d/datoms test/datomic-db :aevt :db/ident))

  "onprem syntax"
  (->> (datoms> test/datomic-db :aevt :db/ident)
       (m/eduction (map datafy) (take 3))
       (m/reduce conj []) m/?)
  := [[0 10 :db.part/db 13194139533312 true]
      [1 10 :db/add 13194139533312 true]
      [2 10 :db/retract 13194139533312 true]]

  "client syntax"
  (->> (datoms> test/datomic-db {:index :aevt, :components [:db/ident]})
       (m/eduction (map datafy) (take 3))
       (m/reduce conj []) m/?)
  := [[0 10 :db.part/db 13194139533312 true]
      [1 10 :db/add 13194139533312 true]
      [2 10 :db/retract 13194139533312 true]])

(defn tx-range>
  ([conn {:keys [start end]}] (tx-range> (d/log conn) start end))
  ([log ?start ?end] (mx/seq-consumer (d/tx-range log ?start ?end))))

(tests
  "control"
  (->> (d/tx-range (d/log test/datomic-conn) nil nil)
       (eduction (map :data) cat (take 1) (map datafy)))
  := [[13194139534312 50 #inst"2015-06-03T02:05:51.541-00:00" 13194139534312 true]]

  "onprem syntax"
  (->> (tx-range> (d/log test/datomic-conn) nil nil)
       (m/eduction (map :data) cat (take 1) (map datafy))
       (m/reduce conj []) m/?)
  := [[13194139534312 50 #inst"2015-06-03T02:05:51.541-00:00" 13194139534312 true]]

  "cloud syntax"
  (->> (tx-range> test/datomic-conn {})
       (m/eduction (map :data) cat (take 1) (map datafy))
       (m/reduce conj []) m/?)
  := [[13194139534312 50 #inst"2015-06-03T02:05:51.541-00:00" 13194139534312 true]])

(defn query [arg-map] (m/sp (d/query arg-map)))

(defn q
  ([q & inputs] (query {:query q :args inputs}))
  #_([arg-map] (query arg-map))) ; collision with zero input call

(tests
  (take 3 (m/? (query {:query '[:find (pull ?e [:db/ident]) ?f
                            :where [?e :db/valueType ?f]]
                       :args [test/datomic-db]})))
  := [[#:db{:ident :db/system-tx} 21]
      [#:db{:ident :db.sys/partiallyIndexed} 24]
      [#:db{:ident :db.sys/reId} 20]])

(defn qseq [query-map] (mx/seq-consumer (d/qseq query-map)))

(tests
  (m/? (->> (qseq {:query '[:find (pull ?e [:db/ident]) ?f
                            :where [?e :db/valueType ?f]]
                   :args [test/datomic-db]})
            (m/eduction (take 3))
            (m/reduce conj [])))
  := [[#:db{:ident :db/system-tx} 21]
      [#:db{:ident :db.sys/partiallyIndexed} 24]
      [#:db{:ident :db.sys/reId} 20]])

(defn history [db] (m/sp (d/history db)))

(defn squuid [] (d/squuid))