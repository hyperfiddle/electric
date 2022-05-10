(ns dev
  (:require
    [datahike.api :as d]
    [datahike.core :as dc]
    [hyperfiddle.api :as hf]
    [triage.logger :as log]
    [hyperfiddle.rcf :refer [tests % !]]))


(defn fixtures [$]
  ; portable
  (-> $
    (d/with [{:db/id 1 :order/type :order/gender :db/ident :order/male}
             {:db/id 2 :order/type :order/gender :db/ident :order/female}])
    :db-after
    (d/with [{:db/id 3 :order/type :order/shirt-size :db/ident :order/mens-small :order/gender :order/male}
             {:db/id 4 :order/type :order/shirt-size :db/ident :order/mens-medium :order/gender :order/male}
             {:db/id 5 :order/type :order/shirt-size :db/ident :order/mens-large :order/gender :order/male}
             {:db/id 6 :order/type :order/shirt-size :db/ident :order/womens-small :order/gender :order/female}
             {:db/id 7 :order/type :order/shirt-size :db/ident :order/womens-medium :order/gender :order/female}
             {:db/id 8 :order/type :order/shirt-size :db/ident :order/womens-large :order/gender :order/female}])
    :db-after
    (d/with [{:db/id 9 :order/email "alice@example.com" :order/gender :order/female :order/shirt-size :order/womens-large
              :order/tags  [:a :b :c]}
             {:db/id 10 :order/email "bob@example.com" :order/gender :order/male :order/shirt-size :order/mens-large
              :order/tags  [:b]}
             {:db/id 11 :order/email "charlie@example.com" :order/gender :order/male :order/shirt-size :order/mens-medium}])
    :db-after
    #_(d/with [{:db/id 12 :order/email "alice@example.com" :order/gender :order/female :order/shirt-size :order/womens-large}
             {:order/email "bob@example.com" :order/gender :order/male :order/shirt-size :order/mens-large}
             {:order/email "charlie@example.com" :order/gender :order/male :order/shirt-size :order/mens-medium}])

    #_:db-after))

;(defn init-datomic []
;  (def schema [{:db/ident :order/email :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
;               {:db/ident :order/gender :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
;               {:db/ident :order/shirt-size :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
;               {:db/ident :order/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
;               {:db/ident :order/tags :db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}])
;  (d/create-database "datomic:mem://hello-world")
;  (def ^:dynamic *$* (-> (d/connect "datomic:mem://hello-world") d/db (d/with schema) :db-after fixtures))
;  #_(alter-var-root #'*$* (constantly $)))

(def schema
  ;; manual db/ids for tests consistency and clarity, not a requirement
  [{:db/id 100001 :db/ident :order/email, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}
   {:db/id 100002 :db/ident :order/gender, :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/id 100003 :db/ident :order/shirt-size, :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/id 100004 :db/ident :order/type, :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
   {:db/id 100005 :db/ident :order/tags, :db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}])

(def db-config {:store {:backend :mem, :id "default"}})

(defn setup-db! []
  (log/info "Initializing Test Database")
  (d/delete-database db-config)
  (d/create-database db-config)
  (def conn (d/connect db-config)) ;; connect to "default"
  (let [$ (-> (d/with (d/db conn) schema)
              :db-after
              (fixtures))
        db (hf/->DB "$" 0 nil $)]
    #?(:clj (alter-var-root #'hf/*db* (constantly db))
       :cljs (set! hf/*db* db))
    #?(:clj (alter-var-root #'hf/*$* (constantly $))
       :cljs (set! hf/*$* $))))

;#?(:clj (init-datomic)
;   :cljs (init-datascript))
#?(:clj (setup-db!))

(def db hf/*$*)                                             ; for @(requiring-resolve 'dev/db)

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
    (dc/touch (d/entity db e))
    ; expected failure
    := {:order/email      "alice@example.com",
        :order/gender     #:db{:id 2},
        :order/shirt-size #:db{:id 8},
        :order/tags       #{:c :b :a},
        :db/id            9})

  (tests
    "entities are not maps"
    (dc/touch (d/entity db e))
    (type *1) := datahike.impl.entity.Entity)               ; not a map

  (tests
    "careful, entity API tests are fragile and (into {}) is insufficient"
    (->> (dc/touch (d/entity db e))                     ; touch is the best way to inspect an entity
         (into {}))                                         ; but it's hard to convert to a map...
    := #:order{#_#_:id 9                                    ; db/id is not present!
               :email      "alice@example.com",
               :gender     _ #_#:db{:id 2},                 ; entity ref not =
               :shirt-size _ #_#:db{:id 8},                 ; entity ref not =
               :tags       #{:c :b :a}}

    "select keys doesn't fix the problem as it's not recursive"
    (-> (dc/touch (d/entity db e))
        (select-keys [:order/email :order/shirt-size :order/gender]))
    := #:order{:email "alice@example.com",
               :shirt-size _ #_#:db{:id 8},                 ; still awkward, need recursive pull
               :gender _ #_#:db{:id 2}})

  "TLDR is use (d/pull ['*]) like the first example"
  (tests
    (d/pull db ['*] :order/female)
    := {:db/id female :db/ident :order/female :order/type :order/gender})

  (tests
    (d/q '[:find [?e ...] :where [_ :order/gender ?e]] db)
    := [2 1] #_[:order/male :order/female])
  )
