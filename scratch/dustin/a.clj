(ns dustin.a
  (:require
    [datomic.api :as d]
    [dustin.dev :refer [*$*]]
    [minitest :refer [tests]]))



(defn genders []
  (d/q '[:find [?e ...]
         :where [_ :dustingetz/gender ?e]]
    *$*))

(defn shirt-sizes [gender]
  (d/q '[:in $ ?gender
         :find [?e ...]
         :where
         [?e :dustingetz/type :dustingetz/shirt-size]
         [?e :dustingetz/gender ?gender]]
    *$* gender))

(defn shirt-size [gender]
  (d/q '[:in $ ?gender
         :find ?e .
         :where
         [?e :dustingetz/type :dustingetz/shirt-size]
         [?e :dustingetz/gender ?gender]]
    *$* gender))

(defn needle-match [v needle]
  (clojure.string/includes?
    (.toLowerCase (or (str v) ""))
    (.toLowerCase (or (str needle) ""))))

(defn submissions [needle]
  (d/q '[:find [?e ...]
         :in $ ?needle
         :where
         [?e :dustingetz/email ?email]
         [(dustin.fiddle/needle-match ?email ?needle)]]
    *$* needle))

(declare hf-pull)

(tests
  (hf-pull
    '[{(submissions needle)
       [:dustingetz/email
        {:dustingetz/gender
         [:db/ident
          {(shirt-sizes dustingetz/gender)
           [:db/ident]}]}]}
      {(genders)
       [:db/ident]}]
    nil
    {'needle "example.com"})
  := '{(submissions needle)
       [{:dustingetz/email "alice@example.com",
         :gender           {:db/ident :dustingetz/female,
                            (shirt-sizes dustingetz/gender)
                                      [{:db/ident :dustingetz/womens-medium}
                                       {:db/ident :dustingetz/womens-large}
                                       {:db/ident :dustingetz/womens-small}]}}
        {:dustingetz/email "bob@example.com",
         :gender           {:db/ident :dustingetz/male,
                            (shirt-sizes dustingetz/gender)
                                      [{:db/ident :dustingetz/mens-small}
                                       {:db/ident :dustingetz/mens-medium}
                                       {:db/ident :dustingetz/mens-large}]}}
        {:dustingetz/email "charlie@example.com",
         :gender           {:db/ident :dustingetz/male,
                            (shirt-sizes dustingetz/gender)
                                      [{:db/ident :dustingetz/mens-small}
                                       {:db/ident :dustingetz/mens-medium}
                                       {:db/ident :dustingetz/mens-large}]}}],
       (genders)
       [#:db{:ident :dustingetz/male} #:db{:ident :dustingetz/female}]})
