(ns contrib.datomic-contrib-2020
  (:require [contrib.datomic-contrib :refer [identify]]
            clojure.set
            [contrib.data :refer [update-existing]]
            [hyperfiddle.rcf :refer [tests]]))

(defn attr-unreverse [a]
  {:pre [(-> (name a) (subs 0 1) (= "_"))]}
  (keyword (namespace a) (-> (name a) (subs 1))))

(tests
  (attr-unreverse :a/_b) := :a/b)

(declare attr)

(defn make-reverse-attr [schema a]
  ; unique scalars can't be pulled in reverse
  ; unique ref doesn't imply that _no other attr_ points to the entityvalue
  ; isComponent implies a 1-1 relationship, so the reverse of an isComponent attr will be cardinality-one
  {:db/ident a
   :db/valueType :db.type/ref
   :db/cardinality (let [{:keys [db/isComponent]} (attr schema (attr-unreverse a))] ; bootstrap
                     (if isComponent ; careful
                       :db.cardinality/one
                       :db.cardinality/many))})

(comment
  (cardinality test.seattle/schema :community/_neighborhood) := :db.cardinality/many)

; :db/id is currently addressable (e.g. user wants to render that column)
(def dbid {:db/ident :db/id
           :db/cardinality :db.cardinality/one
           :db/valueType :db.type/long
           #_#_:db/unique :db.unique/identity})

(def ^:private xf-attr-in-set
  "A specific operation user in `attr`, designed for performance."
  (map (fn [kv]
         (let [v (val kv)] ; avoid destructuring, nth was too slow
           (if (boolean? v) (key kv) v)))))

(defn attr "hydrated attribute map repr - likely private, use the helpers, right?"
  ; rename to attr-map
  ([schema a]
   (when (and a (keyword? a))
     (let [is-reverse-nav (-> (name a) (subs 0 1) (= "_"))]
       (cond
         (= a :db/id) dbid
         is-reverse-nav (make-reverse-attr schema a)
         :else
         (if-let [attr (a schema)] ; #_(.-schema-by-attr #?(:clj schema, :cljs ^js schema))
           (-> attr
               (update-existing :db/valueType identify)
               (update-existing :db/cardinality identify)
               (update-existing :db/isComponent identify)
               (update-existing :db/unique identify))
           #_(attr-spec a f))))))
  ([schema a corcs]
   (let [haystack (into #{} xf-attr-in-set (attr schema a)) ; component
         needles (contrib.data/xorxs corcs #{})]
     (clojure.set/superset? haystack needles)))) ; haystack must have all the needles

(tests
  (attr test.seattle/schema :community/neighborhood)
  := #:db{:ident :community/neighborhood,
          :valueType :db.type/ref,
          :cardinality :db.cardinality/one,
          :doc "A community's neighborhood"}

  (attr test.seattle/schema :community/_neighborhood)
  := #:db{:ident :community/_neighborhood,
          :valueType :db.type/ref,
          :cardinality :db.cardinality/many}

  (attr nil :user/attr-not-defined) := nil)

; These could support unqualified keywords, todo collapse predicate to second arity
(defn cardinality ([schema a] (get (attr schema a) :db/cardinality)) ([schema a k] (= k (cardinality schema a))))
(defn cardinality? [this a k] (cardinality this a k))
(defn isComponent [this a] (get (attr this a) :db/isComponent)) ; follows Datomic naming case conventions
(defn valueType [this a] (get (attr this a) :db/valueType))
(defn valueType? [this a k] (= k (valueType this a)))
(defn ref? [this k] (valueType? this k :db.type/ref))
(defn unique [this a] (get (attr this a) :db/unique))
(defn unique? [this a k] (= k (unique this a)))
(defn identity? [this a] (unique? this a :db.unique/identity))

(defn one? [schema a] (cardinality? schema a :db.cardinality/one))
(defn many? [schema a] (cardinality? schema a :db.cardinality/many))
(defn ref-one? [schema a] (and (valueType? schema a :db.type/ref) (cardinality? schema a :db.cardinality/one)))
(defn ref-many? [schema a] (and (ref? schema a) (cardinality? schema a :db.cardinality/many)))
(defn scalar-one? [schema a] (and (not (ref? schema a)) (cardinality? schema a :db.cardinality/one)))
(defn scalar-many? [schema a] (and (not (ref? schema a)) (cardinality? schema a :db.cardinality/many)))

(tests
  (valueType test.seattle/schema :community/neighborhood) := :db.type/ref
  (valueType? test.seattle/schema :community/neighborhood :db.type/ref) := true
  (valueType? test.seattle/schema :community/neighborhood :db.type/string) := false
  (cardinality test.seattle/schema :community/neighborhood) := :db.cardinality/one
  (cardinality? test.seattle/schema :community/neighborhood :db.cardinality/one) := true
  (ref-one? test.seattle/schema :community/neighborhood) := true
  (unique? test.seattle/schema :district/name :db.unique/identity) := true

  "schema predicates accept reverse attrs"
  (valueType test.seattle/schema :community/_neighborhood) := :db.type/ref
  (cardinality test.seattle/schema :community/_neighborhood) := :db.cardinality/many
  ;(cardinality test.seattle/schema :fiddle/_links) := :db.cardinality/one ; no isComponent test attr in seattle
  )

;(defn find-identity-attr "Search an entity map for a :db.unique/identity attribute, or nil if not found.
;  Does not traverse :db.type/ref nor :db/isComponent."
;  [schema e-map]
;  {:pre [schema]}
;  (->> (keys e-map)
;       (map (juxt identity #(unique? schema % :db.unique/identity)))
;       (filter second)
;       (some first)))
;
;(tests
;  (find-identity-attr test/schema {:a 1 :fiddle/ident :yo}) := :fiddle/ident
;  (find-identity-attr test/schema {:a 1}) := nil
;  (find-identity-attr test/schema nil) := nil)
;
;(defn lookup-ref [schema e-map]
;  (if-let [a (find-identity-attr schema e-map)]
;    [a (a e-map)]))
