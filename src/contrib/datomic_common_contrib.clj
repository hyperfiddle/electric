(ns contrib.datomic-common-contrib
  ; is this contrib.datomic-photon ?
  (:require [contrib.data :refer [index-by unqualify]]
            [hyperfiddle.rcf :refer [tests % tap]]
            [missionary.core :as m]))

; These should use entity API & fetch data when necessary, doing on trees is not ergonomic enough
; (in Hyperfiddle-2020, much complexity stemmed from tree-passing, root cause batch data loading)
(defn identities "select available identifiers from datomic entity, in precedence order"
  [tree] (->> ((juxt :db/ident :db/id) tree)
              (remove nil?)))

(defn identify "infer canonical identity"
  ([tree fallback] (or (identify tree) fallback))
  ([tree] (first (identities tree))))

(tests
  (def tree {:db/id 35435060739965075 :db/ident :release.type/single :release.type/name "Single"})
  (identities tree) := [:release.type/single 35435060739965075]
  (identify tree) := :release.type/single

  (tests
    "these are bad; it should query for the canonical identity, not best locally available identity"
    (def tree2 {:db/id 35435060739965075 :release.type/name "Single"})
    (identities tree2) := [35435060739965075]
    (identify tree2) := 35435060739965075)

  "accept fallback value (like keywords)"
  (identify tree 0) := :release.type/single
  (identify tree2 0) := 35435060739965075
  (identify {} 0) := 0

  "No known identifier is valid"
  (identities {}) := []
  (identify {}) := nil

  (index-by :db/id [tree2])   := {35435060739965075 {:db/id 35435060739965075, :release.type/name "Single"}}
  (index-by identify [tree2]) := {35435060739965075 {:db/id 35435060739965075, :release.type/name "Single"}})

(defn reverse-attr [?kw]
  (if ?kw
    (keyword (namespace ?kw)
             (let [s (name ?kw)]
               (case (.charAt s 0)
                 \_ (subs s 1)
                 (str "_" s))))))

(tests
  (reverse-attr :foo/bar) := :foo/_bar
  (reverse-attr nil) := nil
  (reverse-attr :foo/_bar) := :foo/bar)