(ns contrib.datomic-m
  (:require contrib.deptector
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]))

; Peer API https://docs.datomic.com/on-prem/clojure/index.html#datomic.api/squuid
; Client API https://docs.datomic.com/client-api/index.html
; Client API it seems is the same across all Datomic products (Onprem, Cloud, Ion, Dev local)

(defn detect-datomic-products []
  (->> ['datomic.api
        'datomic.client.api
        'datomic.client.api.async]
       (filter contrib.deptector/ns-available?)
       set))

(comment
  (detect-datomic-products) := #{'datomic.api}
  ('datomic.api #{'datomic.api}) := datomic.api
  ('datomic.client.api #{'datomic.api}) := nil)

(def datomic-products (detect-datomic-products))

; if you have more than one on the classpath, you'll need to set this from userland
;(def ^:dynamic datomic-product (if (= 1 (count datomic-products)) (first datomic-products) nil))

(def tempid?)
(def client)
(def connect)
(def db)
(def db-stats)
(def with)
(def with-db)
(def entity)
(def touch)
(def pull)
(def pull-sorted)
(def datoms>)
(def tx-range>)
(def q)
(def query)
(def qseq)
(def history)
(def squuid)

(defn install-datomic-onprem []
  (require 'contrib.datomic-peer-m)
  (alter-var-root #'tempid?     (constantly (eval 'contrib.datomic-peer-m/tempid?)))
  ; client
  (alter-var-root #'connect     (constantly (eval 'contrib.datomic-peer-m/connect)))
  (alter-var-root #'db          (constantly (eval 'contrib.datomic-peer-m/db)))
  (alter-var-root #'db-stats    (constantly (eval 'contrib.datomic-peer-m/db-stats)))
  (alter-var-root #'with        (constantly (eval 'contrib.datomic-peer-m/with)))
  (alter-var-root #'with-db     (constantly (eval 'contrib.datomic-peer-m/with-db)))
  ; with-db
  (alter-var-root #'entity      (constantly (eval 'contrib.datomic-peer-m/entity)))
  (alter-var-root #'touch       (constantly (eval 'contrib.datomic-peer-m/touch)))
  (alter-var-root #'pull        (constantly (eval 'contrib.datomic-peer-m/pull)))
  (alter-var-root #'pull-sorted (constantly (eval 'contrib.datomic-peer-m/pull-sorted)))
  (alter-var-root #'datoms>     (constantly (eval 'contrib.datomic-peer-m/datoms>)))
  (alter-var-root #'tx-range>   (constantly (eval 'contrib.datomic-peer-m/tx-range>)))
  (alter-var-root #'q           (constantly (eval 'contrib.datomic-peer-m/q)))
  (alter-var-root #'query       (constantly (eval 'contrib.datomic-peer-m/query)))
  (alter-var-root #'qseq        (constantly (eval 'contrib.datomic-peer-m/qseq)))
  (alter-var-root #'history     (constantly (eval 'contrib.datomic-peer-m/history)))
  (alter-var-root #'squuid      (constantly (eval 'contrib.datomic-peer-m/squuid))))

(defn install-datomic-cloud []
  (require 'contrib.datomic-cloud-m)
  (alter-var-root #'tempid?     (constantly (eval 'contrib.datomic-cloud-m/tempid?)))
  (alter-var-root #'connect     (constantly (eval 'contrib.datomic-cloud-m/connect)))
  (alter-var-root #'client      (constantly (eval 'contrib.datomic-cloud-m/client)))
  (alter-var-root #'db          (constantly (eval 'contrib.datomic-cloud-m/db)))
  (alter-var-root #'db-stats    (constantly (eval 'contrib.datomic-cloud-m/db-stats)))
  (alter-var-root #'with        (constantly (eval 'contrib.datomic-cloud-m/with)))
  (alter-var-root #'with-db     (constantly (eval 'contrib.datomic-cloud-m/with-db)))
  ; entity
  ; touch
  (alter-var-root #'pull        (constantly (eval 'contrib.datomic-cloud-m/pull)))
  (alter-var-root #'pull-sorted (constantly (eval 'contrib.datomic-cloud-m/pull-sorted)))
  (alter-var-root #'datoms>     (constantly (eval 'contrib.datomic-cloud-m/datoms>)))
  (alter-var-root #'tx-range>   (constantly (eval 'contrib.datomic-cloud-m/tx-range>)))
  (alter-var-root #'q           (constantly (eval 'contrib.datomic-cloud-m/q)))
  (alter-var-root #'query       (constantly (eval 'contrib.datomic-cloud-m/query)))
  (alter-var-root #'qseq        (constantly (eval 'contrib.datomic-cloud-m/qseq)))
  (alter-var-root #'history     (constantly (eval 'contrib.datomic-cloud-m/history)))
  ; squuid
  )

(defn install-defs! []
  (println "Datomic APIs detected: " (pr-str datomic-products))
  (cond
    (datomic-products 'datomic.api)
    (install-datomic-onprem)

    (datomic-products 'datomic.client.api.async)
    (install-datomic-cloud)))

(install-defs!)

(comment
  (take 3 (keys (m/? (pull test/datomic-db {:eid 50 :selector '[*]}))))
  := [:db/id :db/ident :db/valueType])
