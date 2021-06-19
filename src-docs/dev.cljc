(ns dev
  (:require
    ;#?(:clj [datomic.api :as d])
    [datascript.core :as d]
    [hyperfiddle.api :refer [*$*]]))


(defn fixtures [$]
  ; portable
  (-> $
    (d/with [{:dustingetz/type :dustingetz/gender :db/ident :dustingetz/male}
             {:dustingetz/type :dustingetz/gender :db/ident :dustingetz/female}])
    :db-after
    (d/with [{:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-small :dustingetz/gender :dustingetz/male}
             {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-medium :dustingetz/gender :dustingetz/male}
             {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-large :dustingetz/gender :dustingetz/male}
             {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-small :dustingetz/gender :dustingetz/female}
             {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-medium :dustingetz/gender :dustingetz/female}
             {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-large :dustingetz/gender :dustingetz/female}])
    :db-after
    (d/with [{:dustingetz/email "alice@example.com" :dustingetz/gender :dustingetz/female :dustingetz/shirt-size :dustingetz/womens-large
              :dustingetz/tags  [:a :b :c]}
             {:dustingetz/email "bob@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-large
              :dustingetz/tags  [:b]}
             {:dustingetz/email "charlie@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-medium}])
    :db-after
    (d/with [{:dustingetz/email "alice@example.com" :dustingetz/gender :dustingetz/female :dustingetz/shirt-size :dustingetz/womens-large}
             {:dustingetz/email "bob@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-large}
             {:dustingetz/email "charlie@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-medium}])

    :db-after))

;(defn init-datomic []
;  (def schema [{:db/ident :dustingetz/email :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
;               {:db/ident :dustingetz/gender :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
;               {:db/ident :dustingetz/shirt-size :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
;               {:db/ident :dustingetz/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
;               {:db/ident :dustingetz/tags :db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}])
;  (d/create-database "datomic:mem://hello-world")
;  (def ^:dynamic *$* (-> (d/connect "datomic:mem://hello-world") d/db (d/with schema) :db-after fixtures))
;  #_(alter-var-root #'*$* (constantly $)))

(defn init-datascript []
  (def schema
    {:dustingetz/email      {#_#_:db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
     :dustingetz/gender     {#_#_:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
     :dustingetz/shirt-size {#_#_:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
     :dustingetz/type       {#_#_:db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
     :dustingetz/tags       {#_#_:db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}
     :db/ident              {:db/unique :db.unique/identity}})
  (let [$ (-> (d/create-conn schema) d/db fixtures)]
    #?(:clj (alter-var-root #'*$* (constantly $))
       :cljs (set! *$* $))))

;#?(:clj (init-datomic)
;   :cljs (init-datascript))
(init-datascript)

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
  (d/touch (d/entity *$* alice))
  (d/touch (d/entity *$* [:dustingetz/email "alice@example.com"]))
  => {:db/id                 alice,
      :dustingetz/email      "alice@example.com",
      :dustingetz/gender     :dustingetz/female,
      :dustingetz/shirt-size :dustingetz/womens-large,
      :dustingetz/tags       #{:c :b :a}}

  (d/touch (d/entity *$* :dustingetz/female))
  => {:db/id           female
      :db/ident        :dustingetz/female
      :dustingetz/type :dustingetz/gender}
  )
