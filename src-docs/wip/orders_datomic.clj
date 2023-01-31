(ns wip.orders-datomic
  "query functions used in tee-shirt orders demo"
  (:require [clojure.spec.alpha :as s]
            contrib.str
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tap % tests]]))

(try (require '[datomic.api :as d])
     (catch java.io.FileNotFoundException e
       (throw (ex-info "datomic.api not available, check Datomic pro version >= 1.0.6527" {}))))

(defn fixtures [$]
                                        ; portable
  (-> $
    (d/with [{:db/ident :order/male}
             {:db/ident :order/female}])
    :db-after
    (d/with [{:order/type :order/shirt-size :db/ident :order/mens-small :order/gender :order/male}
             {:order/type :order/shirt-size :db/ident :order/mens-medium :order/gender :order/male}
             {:order/type :order/shirt-size :db/ident :order/mens-large :order/gender :order/male}
             {:order/type :order/shirt-size :db/ident :order/womens-small :order/gender :order/female}
             {:order/type :order/shirt-size :db/ident :order/womens-medium :order/gender :order/female}
             {:order/type :order/shirt-size :db/ident :order/womens-large :order/gender :order/female}])
    :db-after
    (d/with [{:order/email "alice@example.com" :order/gender :order/female :order/shirt-size :order/womens-large
              :order/tags  [:a :b :c]}
             {:order/email "bob@example.com" :order/gender :order/male :order/shirt-size :order/mens-large
              :order/tags  [:b]}
             {:order/email "charlie@example.com" :order/gender :order/male :order/shirt-size :order/mens-medium}])
    :db-after
    #_(d/with [{:db/id 12 :order/email "alice@example.com" :order/gender :order/female :order/shirt-size :order/womens-large}
               {:order/email "bob@example.com" :order/gender :order/male :order/shirt-size :order/mens-large}
               {:order/email "charlie@example.com" :order/gender :order/male :order/shirt-size :order/mens-medium}])

    #_:db-after))

(defn init-datomic []
  (let [schema [{:db/ident :order/email :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                {:db/ident :order/gender :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/ident :order/shirt-size :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/ident :order/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
                {:db/ident :order/tags :db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}]]
    (d/create-database "datomic:mem://hello-world")
    (def ^:dynamic *$* (-> (d/connect "datomic:mem://hello-world") d/db (d/with schema) :db-after fixtures))))


(init-datomic)


(s/fdef genders :args (s/cat) :ret (s/coll-of number?))
(defn genders []
  (into [] (sort (d/q '[:find [?ident ...] :where [_ :order/gender ?e] [?e :db/ident ?ident]] hf/*$*))))

(tests
  (binding [hf/*$* *$*]
    (genders)) := [:order/female :order/male])

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                            :needle string?)
        :ret (s/coll-of number?))

(defn shirt-sizes [gender needle]
  ;; resolve db/id and db/ident genders to same entity datomic does this
  ;; transparently datascript does not
  (sort
    (if gender
      (d/q '[:in $ ?gender ?needle
             :find [?ident ...]
             :where
             [?e :order/type :order/shirt-size]
             [?e :order/gender ?g]
             [?g :db/ident ?gender]
             [?e :db/ident ?ident]  ; remove
             [(contrib.str/includes-str? ?ident ?needle)]]
        hf/*$*
        gender (or needle ""))
      (d/q '[:in $ ?needle
             :find [?e ...]
             :where
             [?e :order/type :order/shirt-size]
             [?e :db/ident ?ident]
             [(contrib.str/includes-str? ?ident ?needle)]]
        hf/*$*
        (or needle "")))))

(tests
  (binding [hf/*$* *$*]
    (shirt-sizes :order/female #_2 "") := [:order/womens-large :order/womens-medium :order/womens-small]
    (shirt-sizes :order/female #_2 "med") := [:order/womens-medium]))

(defn orders [needle]
  (sort (d/q '[:find [?e ...] :in $ ?needle :where
               [?e :order/email ?email]
               [(clojure.string/includes? ?email ?needle)]]
          hf/*$* (or needle ""))))

(tests
  (binding [hf/*$* *$*]
    (orders "") := [17592186045428 17592186045429 17592186045430]
    (orders "example") := [17592186045428 17592186045429 17592186045430]
    (orders "b") := [17592186045429]))

(s/fdef orders :args (s/cat :needle string?)
        :ret (s/coll-of (s/keys :req [:order/email
                                      :order/email1
                                      :order/gender
                                      :order/shirt-size])))

(s/fdef order :args (s/cat :needle string?) :ret number?)
(defn order [needle] (first (orders needle)))

(tests
  (binding [hf/*$* *$*]
    (order "") := 17592186045428
    (order "bob") := 17592186045429))

(s/fdef one-order :args (s/cat :sub any?) :ret any?)
(defn one-order [sub] (hf/*nav!* hf/*$* sub :db/id))


(defn nav! [db e a] (let [v (get (d/entity db e) a)]
                      (prn "nav! datiomic - " e a v)
                      v) )

(defn schema [db a] (when (qualified-keyword? a) (d/entity db a)))
