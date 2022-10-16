(ns contrib.datomic-common-contrib
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

(defn entity-tree-entry-children [schema [k v :as row]] ; row is either a map-entry or [0 {:db/id _}]
  ; This shorter expr works as well but is a bit "lucky" with types in that you cannot see
  ; the intermediate cardinality many traversal. Unclear what level of power is needed here
  ;(cond
  ;  (map? v) (into (sorted-map) v)
  ;  (sequential? v) (index-by identify v))

  ; this controlled way dispatches on static schema to clarify the structure
  (cond
    (contains? schema k)
    (let [x ((juxt (comp unqualify identify :db/valueType)
                   (comp unqualify identify :db/cardinality)) (k schema))]
      (case x
        [:ref :one] (into (sorted-map) v) ; todo lift sort to the pull object
        [:ref :many] (index-by identify v) ; can't sort, no sort key
        nil #_(println `unmatched x))) ; no children

    ; in card :many traversals k can be an index or datomic identifier, like
    ; [0 {:db/id 20512488927800905}]
    ; [20512488927800905 {:db/id 20512488927800905}]
    ; [:release.type/single {:db/id 35435060739965075, :db/ident :release.type/single}]
    (number? k) (into (sorted-map) v)

    () (assert false (str "unmatched tree entry, k: " k " v: " v))))

;(tests
;  ; watch out, test schema needs to match
;  (def schema (m/? (schema! user/datomic-db))) ; not available here
;  (entity-tree-entry-children schema [:geocode/data #:db{:id 17592212633436}]))