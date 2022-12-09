(ns hyperfiddle.nav
  (:require clojure.string
            [datomic.client.api :as d]
            [hyperfiddle.rcf :refer [tests]]))

;(defn normalize-attribute "Turn reverse lookup attrs into normal attrs." [attr]
;  (assert (qualified-keyword? attr))
;  (let [name (name attr)]
;    (if (clojure.string/starts-with? name "_")
;      (keyword (namespace attr) (subs name 1))
;      attr)))
;
;#?(:clj (defn query-schema [db a]
;          (when (qualified-keyword? a)
;            (datomic.api/entity db (normalize-attribute a)))))
;
;#?(:clj (tests
;          (def x (query-schema test/datomic-db :db/ident))
;          (:db/valueType x) := :db.type/keyword
;          (:db/cardinality x) := :db.cardinality/one))
;
;#?(:clj
;   (defn normalize-value [db a v]
;     (when v
;       (let [schema (query-schema db a)
;             ref? (= :db.type/ref (:db/valueType schema))]
;         (case (:db/cardinality schema)
;           (nil :db.cardinality/one)
;           (if ref? (:db/id v) v)
;
;           :db.cardinality/many
;           (if ref?
;             (into (empty v)
;                   (map (fn [e]
;                          (if (instance? datomic.Entity e)
;                            (:db/id e)
;                            e))
;                        v))
;             v))))))

#?(:clj (defn nav [db e a]
          (let [entity (cond #_#_(instance? datomic.Entity e) e
                             (map? e) e
                             #_#_:else (datomic.api/entity db e))
                v (get entity a)]
            v #_(normalize-value db a v))))

#?(:clj
   (tests
     (def admin 17592186682120)
     (datomic.api/touch (datomic.api/entity test/datomic-db admin))
     (nav test/datomic-db admin :db/id) := 17592186682120
     (nav test/datomic-db admin :admin/id) := #uuid"58586c35-ce5c-4bbd-bab0-2eb150e58c89"
     (nav test/datomic-db admin :security/role) := #{:security.role/admin :security.role/superadmin}))