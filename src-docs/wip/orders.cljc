(ns wip.orders
  "query functions used in tee-shirt orders demo"
  (:require #?(:clj [datahike.api :as d])
            [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [! % tests]]
            user)
  #?(:cljs (:require-macros wip.orders)))

(s/fdef genders :args (s/cat) :ret (s/coll-of number?))
(defn genders []
  #?(:clj (into [] (sort (d/q '[:find [?e ...] :where [_ :order/gender ?e]] hf/*$*)))))

(tests
  (def dispose (p/run (! (genders))))
  % := [1 2]
  (dispose))

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
                [(user/includes-str? ?ident ?needle)]]
           hf/*$*
           hf/rules gender (or needle ""))
         (d/q '[:in $ % ?needle
                :find [?e ...]
                :where
                [?e :order/type :order/shirt-size]
                [?e :db/ident ?ident]
                [(user/includes-str? ?ident ?needle)]]
           hf/*$*
           hf/rules (or needle ""))))))

;(tests
;  hf/*$*
;  hf/*$*
;  (p/run (! (shirt-sizes. 2 "")))
;  %)

(defn orders [needle]
  #?(:clj
     (sort
       (d/q '[:find [?e ...]
              :in $ ?needle
              :where
              [?e :order/email ?email]
              [(user/includes-str? ?email ?needle)]]
         hf/*$*
         (or needle "")))))

(tests
  (p/run (! (orders. "")))
  %)

(s/fdef orders :args (s/cat :needle string?)
        :ret (s/coll-of (s/keys :req [:order/email
                                      :order/email1
                                      :order/gender
                                      :order/shirt-size])))

(s/fdef order :args (s/cat :needle string?) :ret number?)
(defn order [needle] (first (orders needle)))

(s/fdef one-order :args (s/cat :sub any?) :ret any?)
(defn one-order [sub] (hf/nav! hf/*$* sub :db/id))

(tests
  (p/run (! (order "")))
  %)

(tests
  (def dispose
    (p/run
      (! (orders ""))
      (! (orders "example"))
      (! (orders "b"))))
  % := [9 10 11]
  % := [9 10 11]
  % := [10]
  (dispose))
