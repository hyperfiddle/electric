(ns user.orders
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [! % tests]])
  #?(:cljs (:require-macros [user.orders :refer [genders shirt-sizes orders]])))


(s/fdef genders :args (s/cat) :ret (s/coll-of number?))
(p/defn genders []
  (into [] (sort ~(hf/q '[:find [?e ...] :where [_ :order/gender ?e]] (:db hf/db)))))

(tests
  (def dispose (p/run (binding [hf/db (hf/->DB "$" 0 nil hf/*$*)] (! (p/$ genders)))))
  % := [1 2]
  (dispose))

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?)
        :ret (s/coll-of number?))

(p/defn shirt-sizes [gender needle]
  (sort
    (if gender
      ~(hf/q '[:in $ % ?gender ?needle
               :find [?e ...]
               :where
               [?e :order/type :order/shirt-size]
               [?e :order/gender ?gender]
               [?e :db/ident ?ident]
               [(hyperfiddle.api/includes-str? ?ident ?needle)]]
             (:db hf/db)
             hf/rules gender (or needle ""))
      ~(hf/q '[:in $ % ?needle
               :find [?e ...]
               :where
               [?e :order/type :order/shirt-size]
               [?e :db/ident ?ident]
               [(hyperfiddle.api/includes-str? ?ident ?needle)]]
             (:db hf/db)
             hf/rules (or needle "")))))

;(tests
;  (:db hf/db)
;  hf/*$*
;  (p/run (! (p/$ shirt-sizes 2 "")))
;  %)

(p/defn orders [needle]
  (sort
    ~(hf/q '[:find [?e ...]
             :in $ ?needle
             :where
             [?e :order/email ?email]
             [(hyperfiddle.api/includes-str? ?email ?needle)]]
           (:db hf/db)
           (or needle ""))))

(tests
  (p/run (binding [hf/db (hf/->DB "$" 0 nil hf/*$*)]
           (! (p/$ orders ""))))
  %)

(s/fdef orders :args (s/cat :needle string?)
        :ret (s/coll-of (s/keys :req [:order/email
                                      :order/email1
                                      :order/gender
                                      :order/shirt-size])))

(s/fdef order :args (s/cat :needle string?) :ret number?)
(p/defn order [needle] (first (p/$ orders needle)))

(tests
  (p/run (! (p/$ user.orders/order "")))
  %)

(tests
  (def dispose
    (p/run
      (! (p/$ orders ""))
      (! (p/$ orders "example"))
      (! (p/$ orders "b"))))
  % := [9 10 11]
  % := [9 10 11]
  % := [10]
  (dispose))
