(ns dev
  (:require
    [datahike.api :as d]
    [datahike.core :as dc]
    [hyperfiddle.api :refer [*$*]]
    [hyperfiddle.dev.logger :as log]))


(defn fixtures [$]
  ; portable
  (-> $
    (d/with [{:db/id 1 :dustingetz/type :dustingetz/gender :db/ident :dustingetz/male}
             {:db/id 2 :dustingetz/type :dustingetz/gender :db/ident :dustingetz/female}])
    :db-after
    (d/with [{:db/id 3 :dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-small :dustingetz/gender :dustingetz/male}
             {:db/id 4 :dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-medium :dustingetz/gender :dustingetz/male}
             {:db/id 5 :dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-large :dustingetz/gender :dustingetz/male}
             {:db/id 6 :dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-small :dustingetz/gender :dustingetz/female}
             {:db/id 7 :dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-medium :dustingetz/gender :dustingetz/female}
             {:db/id 8 :dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-large :dustingetz/gender :dustingetz/female}])
    :db-after
    (d/with [{:db/id 9 :dustingetz/email "alice@example.com" :dustingetz/gender :dustingetz/female :dustingetz/shirt-size :dustingetz/womens-large
              :dustingetz/tags  [:a :b :c]}
             {:db/id 10 :dustingetz/email "bob@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-large
              :dustingetz/tags  [:b]}
             {:db/id 11 :dustingetz/email "charlie@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-medium}])
    :db-after
    #_(d/with [{:db/id 12 :dustingetz/email "alice@example.com" :dustingetz/gender :dustingetz/female :dustingetz/shirt-size :dustingetz/womens-large}
             {:dustingetz/email "bob@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-large}
             {:dustingetz/email "charlie@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-medium}])

    #_:db-after))

;(defn init-datomic []
;  (def schema [{:db/ident :dustingetz/email :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
;               {:db/ident :dustingetz/gender :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
;               {:db/ident :dustingetz/shirt-size :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
;               {:db/ident :dustingetz/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
;               {:db/ident :dustingetz/tags :db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}])
;  (d/create-database "datomic:mem://hello-world")
;  (def ^:dynamic *$* (-> (d/connect "datomic:mem://hello-world") d/db (d/with schema) :db-after fixtures))
;  #_(alter-var-root #'*$* (constantly $)))

(def schema
  ;; manual db/ids for tests consistency and clarity, not a requirement
  [{:db/id 100001 :db/ident :dustingetz/email, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}
   {:db/id 100002 :db/ident :dustingetz/gender, :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/id 100003 :db/ident :dustingetz/shirt-size, :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/id 100004 :db/ident :dustingetz/type, :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
   {:db/id 100005 :db/ident :dustingetz/tags, :db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}])

(def db-config {:store {:backend :mem, :id "default"}})

(defn init-datascript []
  (log/info "Initializing Datascript")
  (d/delete-database db-config)
  (d/create-database db-config)
  (def conn (d/connect db-config)) ;; connect to "default"
  (let [$ (-> (d/with (d/db conn) schema)
              :db-after
              (fixtures))]
    #?(:clj (alter-var-root #'*$* (constantly $))
       :cljs (set! *$* $))))

;#?(:clj (init-datomic)
;   :cljs (init-datascript))
#?(:clj (init-datascript))

(def male    1 #_:dustingetz/male   #_17592186045418)
(def female  2 #_:dustingetz/female #_17592186045419)
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
  (dc/touch (d/entity *$* alice))
  (dc/touch (d/entity *$* [:dustingetz/email "alice@example.com"]))
  => {:db/id                 alice,
      :dustingetz/email      "alice@example.com",
      :dustingetz/gender     :dustingetz/female,
      :dustingetz/shirt-size :dustingetz/womens-large,
      :dustingetz/tags       #{:c :b :a}}

  (dc/touch (d/entity *$* :dustingetz/female))
  => {:db/id           female
      :db/ident        :dustingetz/female
      :dustingetz/type :dustingetz/gender}
  )

(tests
  (datascript.core/q '[:find [?e ...] :where [_ :dustingetz/gender ?e]] *$*)
  := [:dustingetz/male :dustingetz/female])

(tests
  (hf/nav! *$* 9 :dustingetz/email)
  := "alice@example.com"

  (m/? (m/reduce conj (hf/nav 9 :dustingetz/email)))
  := ["alice@example.com"])
