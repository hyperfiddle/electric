(ns wip.orders
  "query functions used in tee-shirt orders demo"
  (:require #?(:clj [datahike.api :as d])
            [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [! % tests]]
            user.util)
  #?(:cljs (:require-macros wip.orders)))

; This namespace is exported to CLJS so that the Photon cljs build can resolve them (todo fix)

(s/fdef genders :args (s/cat) :ret (s/coll-of number?))
(defn genders []
  #?(:clj (into [] (sort (d/q '[:find [?e ...] :where [_ :order/gender ?e]] hf/*$*)))))

(tests
  (genders) := [1 2])

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?)
        :ret (s/coll-of number?))

(defn shirt-sizes [gender needle]
  #?(:clj
     (sort
       (if gender
         (d/q '[:in $ % ?gender ?needle
                :find [?e ...]
                :where
                [?e :order/type :order/shirt-size]
                [?e :order/gender ?gender]
                [?e :db/ident ?ident]
                [(user.util/includes-str? ?ident ?needle)]]
           hf/*$*
           hf/rules gender (or needle ""))
         (d/q '[:in $ % ?needle
                :find [?e ...]
                :where
                [?e :order/type :order/shirt-size]
                [?e :db/ident ?ident]
                [(user.util/includes-str? ?ident ?needle)]]
           hf/*$*
           hf/rules (or needle ""))))))

(tests
  (shirt-sizes 2 "") := [6 7 8]
  (shirt-sizes 2 "med") := [7])

(defn orders [needle]
  #?(:clj
     (sort
       (d/q '[:find [?e ...]
              :in $ ?needle
              :where
              [?e :order/email ?email]
              [(user.util/includes-str? ?email ?needle)]]
         hf/*$*
         (or needle "")))))

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
(defn one-order [sub] (hf/nav! hf/*$* sub :db/id))
