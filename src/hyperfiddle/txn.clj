(ns hyperfiddle.txn
  (:require [contrib.datomic-contrib-2020 :as dx]
            [clojure.set :as set]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests]]))

; Not sure if CLJ only
; we need to resovle queries to fix some edge cases
; but if state is in localstorage, maybe the logic is on the client?
; staging area always immediately updates the database, so if immediately transmitted,
; then state likely should be on the backend - all tabs can now share it -
; but we do not have a feature for this

(defn val->identifier [e]
  (cond (string? e) {:tempid e}
        (number? e) {:db/id e}
        (keyword? e) {:db/ident e}
        (vector? e) (conj {} e)))

(tests
  (val->identifier "a") := {:tempid "a"}
  (val->identifier 123) := {:db/id 123}
  (val->identifier :user/male) := {:db/ident :user/male}
  (val->identifier [:user/id #uuid "123e4567-e89b-12d3-a456-426652340000"])
  := #:user{:id #uuid"123e4567-e89b-12d3-a456-426652340000"})

(defn stmt->identifier [schema stmt]
  (cond
    (and (vector? stmt) (#{:db/add :db/retract :db/cas :db/retractEntity} (first stmt)))
    (let [[_ e a v] stmt]
      (merge {} (val->identifier e)
             (when (dx/identity? schema a) {a v})))

    (map? stmt)
    (let [idents (into {} (filter (fn [[a v]] (dx/identity? schema a)) stmt))
          {id :db/id tempid :tempid} stmt]
      (merge
        idents
        (when (or tempid (string? id)) {:tempid (or tempid id)})
        (when (number? id) {:db/id id})
        (when (vector? id) (conj {:db/id id} id))))))

(tests
  (stmt->identifier test.seattle/schema [:db/add "tempid" :community/name "community"]) := {:tempid "tempid"}
  (stmt->identifier test.seattle/schema [:db/add 1234 :community/name "community"]) := {:db/id 1234}
  (stmt->identifier test.seattle/schema [:db/add "tempid" :neighborhood/name "name"]) := {:tempid "tempid" :neighborhood/name "name"}
  (stmt->identifier test.seattle/schema [:db/add 1234 :neighborhood/name "name"]) := {:db/id 1234 :neighborhood/name "name"})

(declare flatten-map-stmt)

(defn flatten-ref-stmt
  [schema e a v]
  (if (map? v)
    (let [e' (or (:db/id v) ; {:person/name "Earnest"}
                 (str (hash v)))]
      (if (or (dx/isComponent schema a)
              (some (partial dx/unique schema) (keys v)))
        (concat (flatten-map-stmt schema (assoc v :db/id e'))
                [[:db/add e a e']])
        (throw (ex-info "Nested Map Transactions must either be a component, or have a unique attribute" {:attribute a}))))
    [[:db/add e a v]]))

(defn flatten-map-stmt
  "Flatten a single Datomic map-form statement into equivalent vector-form statements. Recursive. See test case."
  [schema m]
  (let [e (stmt->identifier schema m)
        e (:db/id e (:tempid e (str (hash m))))]
    (->> (seq m) ; iterate as seq for compatibility with #Entity
         (filter (fn [[a v]] (not= :db/id a))) ; db/id is virtual attribute, not a statement
         ; Don't need to suupport (id, ident, lookup-ref) because transactor will unify the tempid
         (mapcat (fn [[a v]]

                   (cond
                     (dx/ref-one? schema a) ; :employee/manager
                     (flatten-ref-stmt schema e a v)

                     (dx/ref-many? schema a) ; :person/siblings
                     (mapcat (partial flatten-ref-stmt schema e a) v)

                     (dx/scalar-one? schema a) ; :person/name
                     [[:db/add e a v]] ; "Bob"

                     (dx/scalar-many? schema a) ; :person/liked-tags
                     (if (coll? v)
                       (->> v ; [:movies :ice-cream :clojure]
                            (mapv (fn [v]
                                    [:db/add e a v])))
                       [[:db/add e a v]])
                     :else (throw (ex-info "Flatten Map Statement | Attribute not defined in schema" {:schema schema :attribute a}))))))))

(tests
  (flatten-map-stmt test.person-model/schema test.person-model/bob-map-stmt)
  (count *1) := 19

  (flatten-map-stmt
    test.person-model/schema
    ; map-form txns defined only if child is unique insert or a component
    {:employee/manager {:person/address {:address/zip "1234"}}}) ; illegal tx
  :throws RuntimeException ;#?(:clj RuntimeException :cljs js/Error)
  nil)

(defn flatten-tx
  "Normalize a Datomic transaction by flattening any map-form Datomic statements into tuple-form"
  [schema mixed-form-tx]
  (->> mixed-form-tx
       (mapcat (fn [stmt]
                 (if (map? stmt) ; map-form stmt expands to N vector-form stmts
                   (flatten-map-stmt schema stmt)
                   [stmt])))
       vec))

(tests
  (flatten-tx test.person-model/schema test.person-model/bob-txn)
  (count *1) := 21)

(defn filter-tx
  "run predicate on each stmt of tx, traversing map forms without altering them"
  [schema f? tx]
  (->> tx
       (flatten-tx schema) ; flatten map-form stmts for targetted error messages on forbidden oeav
       (filter (fn [[o :as stmt]]
                 ; stmt can be map-form, or vector form [o e a v], or a function invocation
                 ; function calls share namespace with attributes, so whitelist approach still works
                 (f? stmt)))))

(tests
  (filter-tx test.person-model/schema
             (fn [[o :as stmt]]
               (nil? (#{:db/add} o)))
             test.person-model/bob-txn)
  := [[:db/cas 1 :person/age 41 42]
      [:user.fn/foo 'x 'y 'z 'q 'r]])

(defn remove-tx [schema f? tx]
  (filter-tx schema (complement f?) tx))

(tests
  (remove-tx test.person-model/schema
             (fn [[o :as stmt]] (#{:db/add} o))
             test.person-model/bob-txn)
  := [[:db/cas 1 :person/age 41 42]
      [:user.fn/foo 'x 'y 'z 'q 'r]])

(defn expand-hf-tx [forms]
  (reduce
    (fn [acc [tag & args :as form]]
      (if (symbol? tag)
        (let [var (resolve tag)
              _ (assert var (str "Transaction symbol not found " tag))
              f-tx @var
              _ (assert ifn? (str "Transaction symbol resolved to non-function value"))]
          ; f returns a vector of datoms (Dustin: i think stmts not datoms right?)
          (into acc (apply f-tx args)))
        (conj acc form)))
    []
    forms))

(tests
  (defn test-db-add-txfn [x y z]
    [[:db/add x y z]])

  (expand-hf-tx [[`test-db-add-txfn 1 2 3]])
  := [[:db/add 1 2 3]]

  (expand-hf-tx [[`test-db-add-txfn 1 2 3]
                 [:db/add "asdf" "qwer" :zxcv]])
  := [[:db/add 1 2 3]
      [:db/add "asdf" "qwer" :zxcv]])


(def identifier (comp ::hf/tx-identifier hf/tx-meta))
(def cardinality (comp ::hf/tx-cardinality hf/tx-meta))
(def inverse (comp ::hf/tx-inverse hf/tx-meta))

(defn conflicting? [schema tx0 tx1]
  (let [c0? (::hf/tx-conflicting? (hf/tx-meta schema tx0) (constantly false))
        c1? (::hf/tx-conflicting? (hf/tx-meta schema tx1) (constantly false))]
    (or (c0? tx1) (c1? tx0))))

(tests
  (stmt->identifier test.seattle/schema [:db/add "tempid" :community/name "community"])
  := {:tempid "tempid"}

  (stmt->identifier test.seattle/schema [:db/add 1234 :community/name "community"])
  := {:db/id 1234}

  (stmt->identifier test.seattle/schema [:db/add "tempid" :neighborhood/name "name"])
  := {:tempid "tempid" :neighborhood/name "name"}

  (stmt->identifier test.seattle/schema [:db/add 1234 :neighborhood/name "name"])
  {:db/id 1234 :neighborhood/name "name"})

(defmethod hf/tx-meta :db/add [schema [f e a v :as tx]]
  {::hf/tx-inverse [:db/retract e a v]
   ::hf/tx-cardinality (if (dx/one? schema a)
                         ::hf/one
                         ::hf/many)
   ::hf/tx-identifier (stmt->identifier schema tx)
   ::hf/tx-conflicting? (fn [[tx-fn' _ a' :as tx']]
                          (and (= :db/add tx-fn')
                               (= a a')))})

(defmethod hf/tx-meta :db/retract [schema [f e a v :as tx]]
  {::hf/tx-inverse [:db/add e a v]
   ::hf/tx-cardinality (if (dx/one? schema a)
                         ::hf/one
                         ::hf/many)
   ::hf/tx-identifier (stmt->identifier schema tx)
   ::hf/tx-conflicting? (fn [[tx-fn' _ a' :as tx']]
                          (and (= tx-fn' :db/retract)
                               (= a a')))})

(comment
  (hf/tx-meta test.seattle/schema [:db/retract "-1" :communiity/name ""]))

(defmethod hf/tx-meta :db/cas [schema [_ e a o n]]
  {::hf/tx-inverse [:db/cas e a n o]
   ::hf/tx-cardinality ::hf/one
   ::hf/tx-identifier (val->identifier e)
   ::hf/tx-conflicting? (fn [[tx-fn' _ a' :as tx']]
                          (and (#{:db/add :db/retract} tx-fn')
                               (= a a')))})

(defmethod hf/tx-meta :db/retractEntity [schema [_ e]]
  (let [ident (val->identifier e)]
    {::hf/tx-cardinality ::hf/one
     ::hf/tx-identifier ident
     ::hf/tx-conflicting? (constantly true)}))

(comment
  (hf/tx-meta test.seattle/schema [:db/retractEntity "-1"]))

(defmethod hf/tx-meta :default [schema tx]
  (if (and (map? tx) (:db/id tx))
    {::hf/tx-identifier (:db/id tx)}
    nil))

(defn identifier->e [schema identifier]
  (or (:db/id identifier)
      (:tempid identifier)
      (->> identifier
           (filter (fn [[a v]] (dx/identity? schema a)))
           first)))

(tests
  (identifier->e test.seattle/schema {:db/id 1 :tempid "asdf" :x/y "z" :neighborhood/name "qwer"})
  := 1

  (identifier->e test.seattle/schema {:tempid "-1" :x/y "z" :neighborhood/name "n"})
  := "-1"

  (identifier->e test.seattle/schema {:x/y "z" :neighborhood/name "n"}) ; unique
  := [:neighborhood/name "n"])

(defn unified-identifier [id0 id1]
  (let [ks (set/intersection (set (keys id0)) (set (keys id1)))
        unified-ks (filter #(= (get id0 %) (get id1 %)) ks)]
    (if (empty? unified-ks) nil (select-keys id0 unified-ks))))

(tests
  (unified-identifier {:x 1 :y 2 :z 3} {:x 1 :z 3}) := {:x 1 :z 3}
  (unified-identifier {:x 1 :y 2} {:z 3 :w 4}) := nil
  (unified-identifier {:x 1 :y 2 :z 3} {:x 1 :y 4 :z 5}) := {:x 1})

(defn ideal-idx [identifier ideals]
  (filterv identity (mapv (fn [[id grp] i]
                            (when (unified-identifier identifier id)
                              i)) ideals (range))))

(defn absorb [schema identifier ideal tx]
  (if (map? tx)
    (reduce (fn [[identifier ideal] [a v]]
              (absorb schema identifier ideal [:db/add (identifier->e schema identifier) a v]))
            [identifier ideal] tx)
    (let [{:keys [::hf/tx-inverse ::hf/tx-cardinality ::hf/tx-conflicting? ::hf/tx-special ::hf/tx-identifier]
           :as meta} (hf/tx-meta schema tx)
          tx-cardinality (or tx-cardinality (if tx-conflicting? ::hf/one ::hf/many))
          tx-identifier (if (map? tx-identifier) tx-identifier (val->identifier tx-identifier))
          ideal (or ideal #{})]
      [(merge identifier tx-identifier)
       (if tx-special
         (set (tx-special ideal))
         (if (contains? ideal tx-inverse)
           (disj ideal tx-inverse)
           (apply
             (fn [txs]
               (if (= ::hf/one tx-cardinality)
                 (conj
                   (into #{} (remove (partial conflicting? schema tx) txs))
                   tx)
                 (conj txs tx)))
             [ideal])))])))

(tests
  (absorb test.seattle/schema {:db/id 1} #{}
          {:neighborhood/name "name"
           :neighborhood/district [:distrinct/name "somename"]})
  := [{:db/id 1 :neighborhood/name "name"}
   #{[:db/add 1 :neighborhood/name "name"]
     [:db/add 1 :neighborhood/district [:distrinct/name "somename"]]}]

  (absorb test.seattle/schema {:db/id 1} #{[:db/add 1 :community/name "name"]}
          [:db/add 1 :community/name "qwer"])
  := [{:db/id 1}
      #{[:db/add 1 :community/name "qwer"]}]

  (absorb test.seattle/schema {:db/id 1} #{[:db/add 1 :community/name "name"]}
          [:db/retract 1 :community/name "name"])
  := [{:db/id 1} #{}]

  (absorb test.seattle/schema {:db/id 1} #{} [:db/cas "e" :community/name "asdf" "qwer"])
  := [{:db/id 1 :tempid "e"} #{[:db/cas "e" :community/name "asdf" "qwer"]}]

  (absorb test.seattle/schema {:db/id 1} #{} [:db/retractEntity 1])
  := [{:db/id 1} #{[:db/retractEntity 1]}]

  (def test-foo-fn (fn [] '...))
  (absorb test.seattle/schema {:db/id 1} #{} [test-foo-fn "arg0" 'arg1 :arg2])
  [{:db/id 1} #{[test-foo-fn "arg0" 'arg1 :arg2]}])

(defn absorb-stmt [schema ideals stmt]
  (let [identifier (identifier schema stmt)
        identifier (if (map? identifier) identifier (val->identifier identifier))]
    (if-let [idx (first (ideal-idx identifier ideals))]
      (update ideals idx (fn [[identifier ideal]] (absorb schema identifier ideal stmt)))
      (conj ideals (absorb schema identifier nil stmt)))))

(defn construct
  ([schema tx] (construct schema [] tx))
  ([schema ideals tx] (reduce (partial absorb-stmt schema) ideals tx)))

(defn invalid? [schema tx]
  (or (nil? tx)
      (and (map? tx) (<= (count tx) 1))
      (and (vector? tx)
           (or
             (= :db/id (nth tx 2 nil))
             (let [[txfn e a v] tx]
               (or (= e [a v])
                   (and (= :db/add txfn)
                        (dx/many? schema a)
                        (coll? v)
                        (empty? v))))))))

(tests
  (invalid? test.seattle/schema nil) := true
  (invalid? test.seattle/schema {:singlekey :map}) := true
  (invalid? test.seattle/schema [:some/tx "some-id" :db/id "some-value"]) := true
  (invalid? test.seattle/schema [:some/tx [:a :v] :a :v]) := true)

(defn deconstruct-ideal [schema identifier ideal]
  (let [e (identifier->e schema identifier)]
    (into
      (mapv #(assoc % 1 e) (filter (fn [[tx-fn]] (= :db/add tx-fn)) ideal))
      (remove (fn [[tx-fn]] (= :db/add tx-fn)) ideal))))

(defn ideals->tx [schema ideals]
  (->>
    ideals
    (map (partial apply deconstruct-ideal schema))
    (reduce into [])
    (filterv (complement (partial invalid? schema)))))

(defn remove-dangling-ids [schema tx]
  (let [called-tempids (set (filter string? (map second tx)))]
    (vec
      (remove
        (fn [[tx-fn e a v :as stmt]]
          (and (#{:db/add :db/retract} tx-fn)
               (dx/ref? schema a)
               (string? v)
               (not (contains? called-tempids v))))
        tx))))

(tests
  (remove-dangling-ids test.seattle/schema
                       [[:db/add 1234 :community/neighborhood "dangling id"]
                        [:db/add 1234 :some/attr :some/value]])
  := [[:db/add 1234 :some/attr :some/value]]

  (remove-dangling-ids test.seattle/schema
                       [[:db/add 1234 :community/neighborhood "not dangling id"]
                        [:db/add "not dangling id" :some/attr "some-value"]])
  := [[:db/add 1234 :community/neighborhood "not dangling id"]
      [:db/add "not dangling id" :some/attr "some-value"]])

(defn deconstruct [schema ideals]
  (->> ideals
       (ideals->tx schema)
       (remove-dangling-ids schema)))

(tests
  (set (deconstruct test.seattle/schema [[{:db/id 1} #{[:db/add 1 :attr0 1] [:db/retract 1 :attr1 2] [:db/cas 1 :attr2 "v0" "v1"] [:db/some-custom-fn "arg0" 'arg1 "arge2"]}]]))
  := #{[:db/add 1 :attr0 1]
       [:db/retract 1 :attr1 2]
       [:db/cas 1 :attr2 "v0" "v1"]
       [:db/some-custom-fn "arg0" 'arg1 "arge2"]}

  (set (deconstruct test.seattle/schema [[{:db/id 1} #{[:db/retractEntity 1]}]]))
  := #{[:db/retractEntity 1]})

(defn mappify-add-statements [schema adds]
  (let [add-groups (group-by second adds)]
    (mapv
      (fn [[id group]]
        (reduce
          (fn [acc [_ _ a v]]
            (if (dx/many? schema a)
              (update acc a (fn [x] (if (coll? v)
                                      (into (or x #{}) v)
                                      (conj (or x #{}) v))))
              (assoc acc a v)))
          (cond
            (or (string? id) (number? id)) {:db/id id}

            (vector? id) (let [[a v] id]
                           (if (= a :db/id)
                             {:db/id v}
                             {a v})))
          group))
      add-groups)))

(defn mappify [schema tx]
  (->> (vals (update (group-by first tx) :db/add (partial mappify-add-statements schema)))
       (reduce into [])))

(tests
  (mappify test.seattle/schema [[:db/add "0" :a 1] [:db/add "0" :b 2]])
  := [{:db/id "0" :a 1 :b 2}]

  (mappify test.seattle/schema [[:db/add 0 :a 1] [:db/add 1 :b 2]])
  := [{:db/id 0 :a 1} {:db/id 1 :b 2}])

(defn into-tx [schema tx tx']
  (let [a (construct schema (flatten-tx schema tx))
        b (construct schema a tx')]
    (->> (deconstruct schema b)
         (mappify schema))))

(tests
  (into-tx test.seattle/schema [] []) := []

  (into-tx test.seattle/schema [] [[:db/add 1 :district/name "Southwest"]
                                   [:db/add 1 :district/region 2]])
  := [{:db/id 1, :district/region 2, :district/name "Southwest"}]

  (into-tx test.seattle/schema []
           [[:db/add 1 :district/name "Southwest"]
            [:db/retract 1 :district/name "Southwest"]])
  := []

  (into-tx test.seattle/schema []
           [[:db/add 1 :district/name "Southwest"]
            [:db/add 1 :district/region 2]
            [:db/retract 1 :district/region 2]])
  [{:db/id 1, :district/name "Southwest"}]

  (into-tx test.seattle/schema []
           [[:db/add 1 :district/region 2]
            [:db/add 1 :district/name "Southwest"]
            [:db/retract 1 :district/name "Southwest"]
            [:db/add 1 :district/name ""]])
  := [{:db/id 1, :district/name "", :district/region 2}]

  (into-tx test.seattle/schema [] [[:db/retract 1 :district/region 2]])
  := [[:db/retract 1 :district/region 2]]

  "map-form entity with no concrete attrs (only db/id) is eliminated (not representable)"
  (into-tx test.seattle/schema [] [{:db/id "-1"}])
  := []

  "add-by-lookup-ref"
  (into-tx test.person-model/schema [] [[:db/add [:person/name "1"] :person/liked-tags :foo]])
  := [{:person/name "1" :person/liked-tags #{:foo}}]

  "add-by-db-id"
  (into-tx test.person-model/schema []
           [[:db/add 17592224238619 :person/liked-tags :category/terminated-contract]])
  := [{:db/id 17592224238619, :person/liked-tags #{:category/terminated-contract}}]

  "cardinality-many-idempotency"
  (into-tx test.person-model/schema []
           [[:db/add 17592224238619 :person/liked-tags #{:category/terminated-contract}]])
  := [{:db/id 17592224238619, :person/liked-tags #{:category/terminated-contract}}]

  (into-tx test.seattle/schema []
           [[:db/add 1 :community/type 20]
            [:db/add 1 :community/type 21]])
  := [{:db/id 1, :community/type #{20 21}}]

  "retract-many-cancel-matching-add"
  (into-tx test.seattle/schema []
           [[:db/add 1 :community/type 20]
            [:db/add 1 :community/type 21]
            [:db/retract 1 :community/type 21]])
  := [{:db/id 1, :community/type #{20}}]

  (into-tx test.seattle/schema []
           [[:db/add 1 :community/type 20]
            [:db/retract 1 :community/type 20]])
  := []

  (into-tx test.seattle/schema []
           [[:db/retract 1 :community/type 20]
            [:db/add 1 :community/type 20]])
  := []

  "retract-many-empty-entity-preserve-retract"
  (into-tx test.seattle/schema [] [[:db/retract 1 :community/type 20]])
  := [[:db/retract 1 :community/type 20]]

  "add-many-cancel-matching-retract"
  (into-tx test.seattle/schema []
           [[:db/add 1 :community/type 20]
            [:db/retract 1 :community/type 21]
            [:db/add 1 :community/type 21]])
  := [{:db/id 1, :community/type #{20}}]

  "longer-test-one"
  (into-tx test.seattle/schema []
           [[:db/add 1 :district/region 2]
            [:db/add 1 :district/name "Southwest"]
            [:db/add 2 :community/name "Asdf"]
            [:db/add 2 :community/url "asdf.com"]
            [:db/retract 1 :district/name "Southwest"]
            [:db/add 1 :district/name ""]])
  := [{:db/id 1, :district/name "", :district/region 2}
      {:db/id 2, :community/url "asdf.com", :community/name "Asdf"}]

  "longer-test-many"
  (into-tx test.seattle/schema []
           [[:db/add 1 :community/type 2]
            [:db/add 1 :community/type 2]])
  := [{:db/id 1, :community/type #{2}}]

  (into-tx test.seattle/schema []
           [[:db/retract 1 :community/type 2]
            [:db/retract 1 :community/type 2]])
  := [[:db/retract 1 :community/type 2]]
  (into-tx test.seattle/schema []
           [[:db/retract 1 :community/type 2]
            [:db/retract 1 :community/type 2]
            [:db/add 1 :community/type 2]])
  := [])

(tests "Components"
  (into-tx test.seattle/schema []
           [[:db/add "0" :person/name "Frodo Baggins"]
            [:db/add "1" :person/name "Paragrin Took"]
            [:db/add "2" :person/name "Tom Bombadill"]
            [:db/add 1234 :community/board "0"]
            [:db/add 1234 :community/board "1"]
            [:db/add 1234 :community/board "2"]])
  := [{:db/id "0" :person/name "Frodo Baggins"}
      {:db/id "1" :person/name "Paragrin Took"}
      {:db/id "2" :person/name "Tom Bombadill"}
      {:db/id 1234 :community/board #{"0" "1" "2"}}])

#_
(tests
  "alter an identity attr and use as lookup ref the same tx"
  ; Note: would be better to canonicalize all refs to ids in an early pass
  (into-tx' [] [[:db/add 2 :district/name "n"] ; changing an upsert attr, introduces possible cycle
                [:db/add [:district/name "n"] :x 1]]) ; resolve to 2 via new value
  := [{:db/id 2 :district/name "n" :x 1}])

#_
(tests
  "lookup refs should unify and resolve to canonical id in txns"
  (into-tx' []
            [[:db/add 1 :district/name "d"]
             [:db/add [:district/name "d"] :a 1]
             [:db/retract 1 :b 1]
             [:db/retract [:district/name "d"] :c 1]
             [:db/cas 1 :x 1 2]
             [:db/cas [:district/name "d"] :y 2 1]])
  := [{:db/id 1 :district/name "d"} in harmless way, datomic resolves it
      [:db/retract 1 :b 1]
      [:db/retract 1 :c 1]
      [:db/cas 1 :x 1 2]
      [:db/cas 1 :y 2 1]])

#_
(tests ":db/retractEntity"
  (def ^:deprecated foo-schema
    (->> [{:db/ident :foo
           :db/valueType :db.type/string
           :db/cardinality :db.cardinality/one}

          {:db/ident :bar
           :db/valueType :db.type/string
           :db/cardinality :db.cardinality/one}

          {:db/ident :ref
           :db/valueType :db.type/ref
           :db/cardinality :db.cardinality/one}

          {:db/ident :component
           :db/valueType :db.type/ref
           :db/cardinality :db.cardinality/one
           :db/isComponent true}]
         (contrib.data/index-by :db/ident)))
  ;(dx/attr foo-schema :foo)
  (def into-tx' (partial into-tx foo-schema))

  ; To test :db/retractEntity, the cases are:
  ; all tempids
  ; some tempids some known (in all combinations)
  ; all known

  ":db/retractEntity retracts datoms in the log in addition to canceling staged stmts"
  (into-tx' [{:db/id 1 :foo "foo"}]
            [[:db/retractEntity 1]])
  := [[:db/retractEntity 1]]

  ":db/retractEntity eliminates when the target entity doesn't unify with a known entity in the log"
  ; upsert stmts can unify the tempid to a known id but if there are none then can eliminate.
  ; Careful: I don't see tests for the opposite case where the tempid does unify.
  (into-tx' [{:db/id "-1" :foo "foo"}] ; map form
            [[:db/retractEntity "-1"]]) ; eliminate, -1 doesn't unify to known db entity
  := []

  "retractEntity must resolve lookup refs first"
  (into-tx' []
            [[:db/add 1 :district/name "d"]
             [:db/add [:district/name "d"] :a 1] ; resolve to 1
             [:db/retractEntity 1]]) ; can't eliminate
  := [[:db/retractEntity 1]]

  (into-tx' []
            [[:db/add 1 :district/name "d"]
             [:db/add [:district/name "d"] :a 1] ; resolve to 1
             [:db/retractEntity [:district/name "d"]]]) ; resolve to 1, can't eliminate
  := [[:db/retractEntity 1]]

  "remove parent entity"
  (into-tx' [{:db/id 1 :foo "foo" :ref "-2"}
             {:db/id "-2" :bar "bar"}] ; stands alone
            [[:db/retractEntity 1]])
  := [[:db/retractEntity 1]
      {:db/id "-2", :bar "bar"}]

  (into-tx' [{:db/id "-1" :foo "foo" :ref "-2"}
             {:db/id "-2" :bar "bar"}] ; stands alone
            [[:db/retractEntity "-1"]])
  := [{:db/id "-2" :bar "bar"}]

  "remove parent entity removes child components"
  (into-tx' [{:db/id "-1" :foo "foo" :component {:bar "bar"}}]
            [[:db/retractEntity "-1"]])
  := []

  (into-tx' [{:db/id "-1" :foo "foo" :component "-2"}
             {:db/id "-2" :bar "bar"}]
            [[:db/retractEntity "-1"]])
  := []

  (into-tx' [{:db/id 1 :foo "foo" :component {:bar "bar"}}]
            [[:db/retractEntity 1]]) ; cannot eliminate
  := [[:db/retractEntity 1]]

  (into-tx' [{:db/id 1 :foo "foo" :component "-2"}
             {:db/id "-2" :bar "bar"}]
            [[:db/retractEntity 1]]) ; cannot eliminate
  := [[:db/retractEntity 1]]

  "is this illegal? This component might now have two parents"
  (into-tx' [{:db/id "-1" :foo "foo" :component {:db/id 2 :bar "bar"}}] ; 2 already exists
            [[:db/retractEntity "-1"]])
  := [{:db/id 2, :bar "bar"}] ; can't eliminate, 2 is known

  "remove child entity"
  (into-tx' [{:db/id "-1" :ref "-2" :foo "foo"}
             {:db/id "-2" :bar "bar"}] ; stands alone
            [[:db/retractEntity "-2"]])
  := [{:db/id "-1" :foo "foo"}]

  (into-tx' [{:db/id 1 :ref "-2" :foo "foo"} ; foo survives
             {:db/id "-2" :bar "bar"}]
            [[:db/retractEntity "-2"]])
  := [{:db/id 1, :foo "foo"}]

  (into-tx' [{:db/id "-1" :ref 2 :foo "foo"} ; foo survives
             {:db/id 2 :bar "bar"}]
            [[:db/retractEntity 2]])
  := [{:db/id "-1" :foo "foo"}
      [:db/retractEntity 2]]

  (into-tx' [{:db/id "-1", :ref "-2"} ; eliminate dangling pointer
             {:db/id "-2" :bar "bar"}]
            [[:db/retractEntity "-2"]])
  := []

  (into-tx' [{:db/id 1 :ref "-2"} ; eliminate dangling pointer, {:db/id 1} is no-op
             {:db/id "-2" :bar "bar"}]
            [[:db/retractEntity "-2"]])
  := []

  (into-tx' [{:db/id "-1" :ref 2} ; eliminate dangling pointer
             {:db/id 2 :bar "bar"}]
            [[:db/retractEntity 2]]) ; known
  := [[:db/retractEntity 2]]

  ; todo: remove child entity with tempid that upserts to known entity


  "parent eliminated on child component removal if now empty"

  "parent survives removing child component"

  (into-tx' [{:db/id 1 :component {:db/id 2 :bar "bar"}}] ; weird
            [[:db/retractEntity 2]])
  := [[:db/retractEntity 2]] ; can't eliminate

  (into-tx' [{:db/id 1 :component {:db/id "-2" :bar "bar"}}]
            [[:db/retractEntity "-2"]])
  := []

  (into-tx' [{:db/id 1 :component "-2"} ; weird, why not canonicalized?
             {:db/id "-2" :bar "bar"}]
            [[:db/retractEntity "-2"]])
  := []

  (into-tx' [{:db/id "-1" :component {:db/id 2 :bar "bar"}}] ; weird, possibly multiple component owners
            [[:db/retractEntity 2]])
  := [[:db/retractEntity 2]]

  (into-tx' [{:db/id "-1" :component 2} ; weird, why not canonicalized?
             {:db/id 2 :bar "bar"}]
            [[:db/retractEntity 2]]) ; retract child component
  := [[:db/retractEntity 2]]

  (into-tx' '[{:db/id "-1" :component {:db/id "-2" :bar "bar"} :foo "foo"}] ; named component
            [[:db/retractEntity "-2"]]) ; retract child component
  := [{:db/id "-1" :foo "foo"}] ; foo survives

  (into-tx' [{:db/id "-1" :component {:bar "bar"} :foo "foo"}] ; anon component
            [[:db/retractEntity "-1"]]) ; retract parent
  := []

  (into-tx' [{:db/id "-1" :component 2 :foo "foo"}
             {:db/id 2 :bar "bar"}]
            [[:db/retractEntity 2]])
  := [{:db/id "-1" :foo "foo"} ; foo survives
      [:db/retractEntity 2]] ; can't eliminate

  (into-tx' [{:db/id 1 :component "-2" :foo "foo"}
             {:db/id "-2" :bar "bar"}]
            [[:db/retractEntity "-2"]])
  := [[:db/add 1 :foo "foo"]]

  (into-tx' [{:db/id "-1" :component "-2" :foo "foo"} ; weird, why not canonicalized?
             {:db/id "-2" :bar "bar"}]
            [[:db/retractEntity "bar"]])
  := [{:db/id "-1" :foo "foo"}] ; foo survives

  (into-tx' '[{:db/id "-1" :component "-2"} ; weird, why not canonicalized?
              {:db/id "-2" :bar "bar"}]
            [[:db/retractEntity "-2"]])
  := []) ; eliminate dangling pointer

#_
(tests "orphaned statements"
  (into-tx' [{:db/id "-1" :ref "-2"}
             {:db/id "-2" :component "-3"}
             {:db/id "-3" :foo "foo"}]
            [[:db/retractEntity "-3"]]) ; only made possible by the non-canonical repr
  := []

  (into-tx' [{:db/id "-1" :ref "-2"}
             {:db/id "-2" :component {:db/id "-2" :foo "foo"}}] ; note that :db/id is optional!
            ; should it sever the component relationship and orphan an entity? unclear
            [[:db/retract "-2" :component "-2"]])
  := [{:db/id "-1", :ref "-2"}]

  (into-tx' [[:db/add "-1" :ref "-2"]
             [:db/add "-2" :ref "-3"]
             [:db/add "-3" :ref "-4"]
             [:db/add "-4" :foo "foo"]]
            [[:db/retractEntity "-4"]])
  := []

  (into-tx' [[:db/add "-1" :ref "-2"]
             [:db/add "-2" :ref "-3"]
             [:db/add "-3" :ref 4]
             [:db/add 4 :foo "foo"]]
            [[:db/retractEntity 4]])
  := [[:db/retractEntity 4]])

(defn minimal-tx [db tx] tx)