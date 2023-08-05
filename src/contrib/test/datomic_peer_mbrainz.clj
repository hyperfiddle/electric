(ns contrib.test.datomic-peer-mbrainz
  (:require #_[contrib.datomic-contrib :as dx] ; no cycle
            [datomic.api :as d]
            [hyperfiddle.rcf :refer [tests]]))

(def ^:dynamic conn)
(def ^:dynamic db)
(def ^:dynamic schema)
(def ^:dynamic fixtures []) ; local datomic tx to rebase onto the db

(def pour-lamour 17592186058336)
(def cobblestone 17592186068764)

(defn install-test-state []
  (alter-var-root #'conn (constantly (d/connect "datomic:dev://localhost:4334/mbrainz-1968-1973")))
  (assert (some? conn))

  (alter-var-root #'db (constantly (:db-after (d/with (d/db conn) fixtures))))
  (assert (some? db))

  #_#_
  (alter-var-root #'schema (constantly (m/? (dx/schema! db))))
  (assert (some? schema)))

(install-test-state)

(tests
  ;(some? schema) := true

  (d/pull db ['*] pour-lamour)
  := {:db/id 17592186058336,
      :abstractRelease/gid #uuid "f05a1be3-e383-4cd4-ad2a-150ae118f622",
      :abstractRelease/name "Pour l’amour des sous / Parle au patron, ma tête est malade",
      :abstractRelease/type #:db{:id 17592186045427},
      :abstractRelease/artists [#:db{:id 580542139477874} #:db{:id 778454232478138}],
      :abstractRelease/artistCredit "Jean Yanne & Michel Magne"}

  (d/pull db ['*] cobblestone)
  := {:db/id 17592186068764,
      :label/gid #uuid "066474f9-fad7-48dd-868f-04d8bb0a5253",
      :label/name "Cobblestone",
      :label/sortName "Cobblestone",
      :label/type #:db{:id 17592186045475},
      :label/country #:db{:id 17592186045691},
      :label/startYear 1972}
  nil)