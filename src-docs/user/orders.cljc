(ns user.orders
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [! % tests]])
  #?(:cljs (:require-macros [user.orders :refer [genders shirt-sizes order orders]])))


(s/fdef genders :args (s/cat) :ret (s/coll-of number?))
(p/defn genders []
  (into [] (sort (new (hf/q '[:find [?e ...] :where [_ :order/gender ?e]] hf/*$*)))))

(tests
  (def dispose (p/run (! (genders.))))
  % := [1 2]
  (dispose))

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?)
        :ret (s/coll-of number?))

(p/defn shirt-sizes [gender needle]
  (sort
    (new (if gender
           (hf/q '[:in $ % ?gender ?needle
                   :find [?e ...]
                   :where
                   [?e :order/type :order/shirt-size]
                   [?e :order/gender ?gender]
                   [?e :db/ident ?ident]
                   [(hyperfiddle.api/includes-str? ?ident ?needle)]]
             hf/*$*
             hf/rules gender (or needle ""))
           (hf/q '[:in $ % ?needle
                   :find [?e ...]
                   :where
                   [?e :order/type :order/shirt-size]
                   [?e :db/ident ?ident]
                   [(hyperfiddle.api/includes-str? ?ident ?needle)]]
             hf/*$*
             hf/rules (or needle ""))))))

;(tests
;  hf/*$*
;  hf/*$*
;  (p/run (! (shirt-sizes. 2 "")))
;  %)

(p/defn orders [needle]
  (sort
    (new (hf/q '[:find [?e ...]
                 :in $ ?needle
                 :where
                 [?e :order/email ?email]
                 [(hyperfiddle.api/includes-str? ?email ?needle)]]
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
(p/defn order [needle] (first (orders. needle)))

(s/fdef sub-profile :args (s/cat :sub any?) :ret any?)
(p/defn sub-profile [sub] (new (hf/nav hf/*$* sub :db/id)))

(tests
  (p/run (! (order. "")))
  %)

(tests
  (def dispose
    (p/run
      (! (orders. ""))
      (! (orders. "example"))
      (! (orders. "b"))))
  % := [9 10 11]
  % := [9 10 11]
  % := [10]
  (dispose))
