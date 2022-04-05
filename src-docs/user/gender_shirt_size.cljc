(ns user.gender-shirt-size
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [! % tests]])
  #?(:cljs (:require-macros [user.gender-shirt-size :refer [genders gender shirt-sizes submissions submission emails sub-profile]])))


(s/fdef genders :args (s/cat) :ret (s/coll-of number?))
(p/defn genders []
  (into [] ~ (hf/q '[:find [?e ...] :where [_ :order/gender ?e]] (:db hf/db))))

(tests
  (def dispose (p/run (! (p/$ genders))))
  % := [:order/male :order/female]
  (dispose))

(p/defn gender []
  (first (p/$ genders)))


(tests
  (def dispose (p/run (! (p/$ gender))))
  % := :order/male
  (dispose))

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?)
        :ret (s/coll-of number?))

(p/defn shirt-sizes [gender needle]
  (when gender
    (sort
     ~(hf/q '[:in $ % ?gender ?needle
              :find [?e ...]
              :where
              [?e :order/type :order/shirt-size]
              [?e :order/gender ?gender]
              [?e :db/ident ?ident]
              (hyperfiddle.api/needle-match ?ident ?needle)]
            (:db hf/db)
            hf/rules gender (or needle "")))))

(p/defn emails [needle]
  ~(hf/q '[:in $ % ?needle
           :find [?e ...]
           :where
           [?e :order/email ?email]
           (hyperfiddle.api/needle-match ?email ?needle)]
         (:db hf/db)
         hf/rules (or needle "")))

(s/fdef emails :args (s/cat :needle string?)
        :ret (s/coll-of string?))

(p/defn submissions [needle _]
  (sort
    ~(hf/q '[:find [?e ...]
          :in $ % ?needle
          :where
          [?e :order/email ?email]
          (hyperfiddle.api/needle-match ?email ?needle)]
          (:db hf/db)
          hf/rules (or needle ""))))

(s/fdef submissions :args (s/cat :needle string?
                                 :reverse? boolean?)
        :ret (s/coll-of (s/keys :req [:order/email
                                      :order/email1
                                      :order/gender
                                      :order/shirt-size])))


(s/fdef submission :args (s/cat :needle string?) :ret number?)
(p/defn submission [needle] (first (p/$ submissions needle)))


(s/fdef sub-profile :args (s/cat :sub any?) :ret any?)
(p/defn sub-profile [sub] ~(hf/nav (:db hf/db) sub :db/id))

(tests
  (hyperfiddle.photon/run (! (p/$ user.gender-shirt-size/submission "")))
  %)

(tests
  (def dispose
    (p/run
      (! (p/$ submissions ""))
      (! (p/$ submissions "example"))
      (! (p/$ submissions "b"))))
  % := [9 10 11]
  % := [9 10 11]
  % := [10]
  (dispose))
