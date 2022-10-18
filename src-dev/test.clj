(ns test
  (:require [contrib.datomic-contrib :as dx]
            [contrib.datomic-m :as d]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]))

(def ^:dynamic datomic-conn)
(def ^:dynamic datomic-db)
(def ^:dynamic schema)
(def ^:dynamic fixtures []) ; local datomic tx to rebase onto the db

(def pour-lamour 87960930235113)
(def cobblestone 536561674378709)

(defn install-test-state [datomic-client]
  (alter-var-root #'datomic-conn (constantly (m/? (d/connect datomic-client {:db-name "mbrainz-subset"}))))
  (alter-var-root #'fixtures (constantly fixtures))

  (alter-var-root #'datomic-db (constantly (:db-after (d/with (d/db datomic-conn) fixtures))))
  (alter-var-root #'schema (constantly (m/? (dx/schema! datomic-db)))))

(tests
  (some? schema) := true

  (m/? (d/pull test/datomic-db {:eid pour-lamour :selector ['*]}))
  := {:db/id 87960930235113,
      :abstractRelease/gid #uuid"f05a1be3-e383-4cd4-ad2a-150ae118f622",
      :abstractRelease/name "Pour l’amour des sous / Parle au patron, ma tête est malade",
      :abstractRelease/type #:db{:id 35435060739965075, :ident :release.type/single},
      :abstractRelease/artists [#:db{:id 20512488927800905}
                                #:db{:id 68459991991856131}],
      :abstractRelease/artistCredit "Jean Yanne & Michel Magne"}

  (m/? (d/pull test/datomic-db {:eid cobblestone :selector ['*]}))
  := {:db/id 536561674378709,
      :label/gid #uuid"066474f9-fad7-48dd-868f-04d8bb0a5253",
      :label/name "Cobblestone",
      :label/sortName "Cobblestone",
      :label/type #:db{:id 44604987715616963, :ident :label.type/originalProduction},
      :label/country #:db{:id 63793664643563930, :ident :country/US},
      :label/startYear 1972}
  nil)