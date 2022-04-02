(ns dev
  (:require
    [datahike.api :as d]
    [datahike.core :as dc]
    [hyperfiddle.api :refer [*$*]]
    [hyperfiddle.dev.logger :as log]))


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
  (dc/touch (d/entity *$* alice))
  (dc/touch (d/entity *$* [:order/email "alice@example.com"]))
  => {:db/id                 alice,
      :order/email      "alice@example.com",
      :order/gender     :order/female,
      :order/shirt-size :order/womens-large,
      :order/tags       #{:c :b :a}}

  (dc/touch (d/entity *$* :order/female))
  => {:db/id           female
      :db/ident        :order/female
      :order/type :order/gender}
  )

(comment
  (tests
   (datascript.core/q '[:find [?e ...] :where [_ :order/gender ?e]] *$*)
   := [:order/male :order/female])

  (tests
   (hf/nav! *$* 9 :order/email)
   := "alice@example.com"

   (m/? (m/reduce conj (hf/nav 9 :order/email)))
   := ["alice@example.com"]))
