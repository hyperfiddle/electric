(ns contrib.datomic-cloud-m
  (:require [contrib.data :refer [omit-keys-ns auto-props]]
            [clojure.core.protocols :as ccp :refer [nav]]
            [clojure.datafy :refer [datafy]]
            datomic.client.api ; remove
            [datomic.client.api.async :as d]
            [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests tap %]])
  (:import (datomic.core.db Datum)))

(defn tempid? [x] (or (string? x) (neg? x)))

(tests
  (tempid? -1) := true
  (tempid? 1) := false
  (tempid? 0) := false
  (tempid? "a") := true)

(defn client [arg-map] (datomic.client.api/client arg-map))

(defn connect [client arg-map] (datomic.client.api/connect client arg-map))

(defn db [conn] (d/db conn))

(extend-protocol ccp/Datafiable
  Datum
  (datafy [^Datum [e a v tx op]] [e a v tx op]))

;(defn entity! [db e] (reify ...))
;(defn touch! [!e] (pull! (d/entity-db !e) (:db/id !e) ['*]))

(defn db-stats [db] (p/chan-read! (d/db-stats db)))

(comment (m/? (db-stats user/db)))

(defn pull
  ([db {:keys [selector eid]}] (pull db selector eid))
  ([db pattern eid]
   (if (tempid? eid)
     (m/sp (if (some #{:db/id} pattern) {:db/id eid} {}))
     (m/via m/blk (d/pull db pattern eid)))))

(tests
  "control - datomic operators work on number tempids"
  (pr-str (d/entity user/datomic-db -1)) := (pr-str {:db/id -1}) ; :db/id is virtual key so test print repr
  (d/pull user/datomic-db [:db/id] -1) := #:db{:id -1}
  (d/pull user/datomic-db ['*] -1) := #:db{:id -1}

  "control - datomic operators crash on string tempids, wtf"
  (d/entity user/datomic-db "tempid-1") :throws datomic.impl.Exceptions$IllegalArgumentExceptionInfo
  (d/pull user/datomic-db [:db/id] "a") :throws datomic.impl.Exceptions$IllegalArgumentExceptionInfo
  (d/pull user/datomic-db ['*] "a") :throws datomic.impl.Exceptions$IllegalArgumentExceptionInfo

  "hyperfiddle needs this defined to represent empty forms"
  (m/? (entity user/datomic-db "tempid-1")) := #:db{:id "tempid-1"}
  (m/? (pull user/datomic-db [:db/id] "a")) := {:db/id "a"}
  (m/? (pull user/datomic-db [:db/ident] "a")) := {})

(defn pull-sorted
  ([db {:keys [selector eid] :as arg-map}] (pull-sorted db selector eid arg-map #_(dissoc arg-map :selector :eid)))
  ([db pattern eid & [arg-map]]
   (let [{:keys [::compare]} (auto-props arg-map)
         arg-map (omit-keys-ns (namespace ::this) arg-map)]
     (m/sp (let [tree (m/? (p/chan-read! (d/pull db arg-map)))]
             (if compare
               ; Use datafy/nav to sort on the fly? or pre-sort here?
               ; need to know cardinality many attrs and sort on nav
               ; unless we can use datoms API to make datomic sort them
               (m/? (m/via m/blk (into (sorted-map-by compare) tree)))
               tree))))))

(comment
  (def cobblestone 536561674378709)
  "pulls are sorted at top layer"
  (take 3 (keys (m/? (d/pull! user/db {:eid cobblestone :selector '[*]}))))
  := [:db/id :label/country :label/gid] ; sorted!

  "pulls are sorted at intermedate layers"
  todo)

(defn datoms> [db arg-map]
  (m/ap (m/?> (m/eduction cat (p/chan->ap (d/datoms db arg-map))))))

(comment
  (time (take 3 (m/? (m/reduce conj [] (datoms> user/db {:index :aevt, :components [:db/ident]})))))
  (time (m/? (m/reduce conj [] (m/eduction (take 3) (datoms> user/db {:index :aevt, :components [:db/ident]})))))
  (time (m/? (m/reduce conj [] (m/eduction (take 3) (datoms> user/db {:index :aevt, :components [:db/txInstant]}))))))

(tests
  (->> (datoms> user/db {:index :aevt, :components [:db/txInstant]})
       (m/eduction (map datafy))
       (m/eduction (take 1))
       (m/reduce conj ()) m/?)
  := [[?tx 50 _ ?tx true]])

(defn tx-range> [conn arg-map] ; has pagination
  (m/ap (m/?> (p/chan->ap (d/tx-range conn arg-map)))))

(tests
  "first datom"
  (->> (tx-range> user/datomic-conn {:start 0, :end 1})
       (m/eduction (map :data) cat (map datafy))
       (m/reduce conj ()) m/? (take 1))
  := [[0 10 :db.part/db 13194139533312 true]])

(defn q [arg-map] (->> (p/chan->ap (d/q arg-map))
                       (m/eduction cat)
                       (m/reduce conj [])))

(def query q)

(comment
  (def query-attrs '[:find (pull ?e [:db/ident]) ?f :where [?e :db/valueType ?f]])
  (m/? (q {:query query-attrs :args [user/db]}))
  := _)

(defn qseq [arg-map] (->> (p/chan->ap (d/qseq arg-map))
                          (m/eduction cat))) ; qseq returns chunks, smooth them out

(tests
  (m/? (->> (qseq {:query '[:find (pull ?e [:db/ident]) ?f :where [?e :db/valueType ?f]]
                   :args [user/db]})
            (m/eduction (take 3))
            (m/reduce conj []))))

(defn history [db] (m/sp (d/history db)))
