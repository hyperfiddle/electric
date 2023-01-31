(ns wip.orders-datascript
  "query functions used in tee-shirt orders demo"
  (:require [clojure.spec.alpha :as s]
            clojure.string
            contrib.str
            [datascript.core :as d]
            [datascript.impl.entity :as de]  ; for `entity?` predicate
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tap % tests]]))

(s/fdef genders :args (s/cat) :ret (s/coll-of number?))
(defn genders []
  (into [] (sort (d/q '[:find [?e ...] :where [_ :order/gender ?e]] hf/*$*))))

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?)
        :ret (s/coll-of number?))

(defn shirt-sizes [gender needle]
  ; resolve db/id and db/ident genders to same entity
  ; datomic does this transparently
  ; datascript does not
  (sort
    (if gender
      (d/q '[:in $ ?gender ?needle
             :find [?e ...]
             :where
             [?e :order/type :order/shirt-size]
             [?e :order/gender ?g]
             [?g :db/ident ?gender]
             [?e :db/ident ?ident]  ; remove
             [(name ?ident) ?nm]
             [(contrib.str/includes-str? ?nm ?needle)]]
           hf/*$*
           gender (or needle ""))
      (d/q '[:in $ ?needle
             :find [?e ...]
             :where
             [?e :order/type :order/shirt-size]
             [?e :db/ident ?ident]
             [(name ?ident) ?nm]
             [(contrib.str/includes-str? ?nm ?needle)]]
           hf/*$*
           (or needle "")))))

(tests
  (shirt-sizes :order/female #_2 "") := [6 7 8]
  (shirt-sizes :order/female #_2 "med") := [7]
  (shirt-sizes :order/female #_2 "d") := [7])

(defn orders [needle]
  (sort (d/q '[:find [?e ...] :in $ ?needle :where
               [?e :order/email ?email]
               [(clojure.string/includes? ?email ?needle)]]
             hf/*$* (or needle ""))))

(tests
  (orders "") := [9 10 11]
  (orders "example") := [9 10 11]
  (orders "b") := [10])

(s/fdef orders :args (s/cat :needle string?)
        :ret (s/coll-of (s/keys :req [:order/email
                                      :order/email1
                                      :order/gender
                                      :order/shirt-size])))

(s/fdef order :args (s/cat :needle string?) :ret number?)
(defn order [needle] (first (orders needle)))

(tests
  (order "") := 9
  (order "bob") := 10)

(s/fdef one-order :args (s/cat :sub any?) :ret any?)
(defn one-order [sub] (hf/*nav!* hf/*$* sub :db/id))

(defn schema [db a] (get (:schema db) a))

(defn nav!
  ([_ e] e)
  ([db e a] (let [v (a (if (de/entity? e) e (d/entity db e)))]
              (if (de/entity? v)
                (:db/id v)
                v)))
  ([db e a & as] (reduce (partial nav! db) (nav! db e a) as)))
