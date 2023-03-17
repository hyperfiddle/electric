;; An example databsae of tee-shirt orders
;; server side only - for demos
(ns user.example-datascript-db
  (:require [datascript.core :as d]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests % tap]]))

(def schema ; user orders a tee-shirt and select a tee-shirt gender and size + optional tags
  ;; :hf/valueType is an annotation, used by hyperfiddle to auto render a UI (To be improved. Datascript only accepts :db/valueType on refs)
  {:order/email      {:hf/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}
   :order/gender     {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one}
   :order/shirt-size {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one}
   :order/type       {:db/cardinality :db.cardinality/one}
   :order/tags       {:db/cardinality :db.cardinality/many}
   :db/ident         {:db/unique :db.unique/identity, :hf/valueType :db.type/keyword}})

(defn fixtures [db]
  (-> db
    ;;  Add tee-shirt types
    (d/with [{:db/id 1, :order/type :order/gender, :db/ident :order/male} ; straight cut
             {:db/id 2, :order/type :order/gender, :db/ident :order/female}]) ; fitted
    :db-after
    ;; Add tee-shirt sizes by type
    (d/with [{:db/id 3 :order/type :order/shirt-size :db/ident :order/mens-small :order/gender :order/male}
             {:db/id 4 :order/type :order/shirt-size :db/ident :order/mens-medium :order/gender :order/male}
             {:db/id 5 :order/type :order/shirt-size :db/ident :order/mens-large :order/gender :order/male}
             {:db/id 6 :order/type :order/shirt-size :db/ident :order/womens-small :order/gender :order/female}
             {:db/id 7 :order/type :order/shirt-size :db/ident :order/womens-medium :order/gender :order/female}
             {:db/id 8 :order/type :order/shirt-size :db/ident :order/womens-large :order/gender :order/female}])
    :db-after
    ;; Add example orders
    (d/with [{:db/id 9,  :order/email "alice@example.com",   :order/gender :order/female,  :order/shirt-size :order/womens-large, :order/tags  [:a :b :c]}
             {:db/id 10, :order/email "bob@example.com",     :order/gender :order/male,    :order/shirt-size :order/mens-large,   :order/tags  [:b]}
             {:db/id 11, :order/email "charlie@example.com", :order/gender :order/male,    :order/shirt-size :order/mens-medium}])
    :db-after))

(declare conn)

(defn setup-db! []
  (def conn (d/create-conn schema))
  (alter-var-root #'hf/*$* (constantly (fixtures (d/db conn)))))

(setup-db!)


(def db hf/*$*) ; for @(requiring-resolve 'user.example-datascript-db/db)

(def male    1 #_:order/male   #_17592186045418)
(def female  2 #_:order/female #_17592186045419)
(def m-sm    3  #_17592186045421)
(def m-md    4  #_nil)
(def m-lg    5  #_nil)
(def w-sm    6  #_nil)
(def w-md    7  #_nil)
(def w-lg    8  #_nil)
(def alice   9  #_17592186045428)
(def bob     10 #_nil)
(def charlie 11 #_nil)

(comment
  (hyperfiddle.rcf/enable!))

(tests
  (def e [:order/email "alice@example.com"])

  (tests
    "(d/pull ['*]) is best for tests"
    (d/pull db ['*] e)
    := {:db/id            9,
        :order/email      "alice@example.com",
        :order/shirt-size #:db{:id 8},
        :order/gender     #:db{:id 2}
        :order/tags [:a :b :c]})

  (comment #_tests
    "careful, entity type is not= to equivalent hashmap"
    (d/touch (d/entity db e))
    ; expected failure
    := {:order/email      "alice@example.com",
        :order/gender     #:db{:id 2},
        :order/shirt-size #:db{:id 8},
        :order/tags       #{:c :b :a},
        :db/id            9})

  (tests
    "entities are not maps"
    (type (d/touch (d/entity db e)))
    *1 := datascript.impl.entity.Entity)             ; not a map

  (comment #_tests
    "careful, entity API tests are fragile and (into {}) is insufficient"
    (->> (d/touch (d/entity db e))                      ; touch is the best way to inspect an entity
         (into {}))                                         ; but it's hard to convert to a map...
    := #:order{#_#_:id 9                                    ; db/id is not present!
               :email      "alice@example.com",
               :gender     _ #_#:db{:id 2},                 ; entity ref not =, RCF can’t unify with entities
               :shirt-size _ #_#:db{:id 8},                 ; entity ref not =
               :tags       #{:c :b :a}}

    "select keys doesn't fix the problem as it's not recursive"
    (-> (d/touch (d/entity db e))
        (select-keys [:order/email :order/shirt-size :order/gender]))
    := #:order{:email "alice@example.com",
               :shirt-size _ #_#:db{:id 8},                 ; still awkward, need recursive pull
               :gender _ #_#:db{:id 2}})                    ; RCF can’t unify with an entities

  "TLDR is use (d/pull ['*]) like the first example"
  (tests
    (d/pull db ['*] :order/female)
    := {:db/id female :db/ident :order/female :order/type :order/gender})

  (tests
    (d/q '[:find [?e ...] :where [_ :order/gender ?e]] db)
    := [2 1] #_[:order/male :order/female])
  )
