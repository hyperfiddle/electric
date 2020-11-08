(ns dustin.fiddle
  (:require
    [datomic.api :as d]
    [dustin.dev :refer [*$*]]
    [minitest :refer [tests]]))


(defn genders []
  (d/q '[:find [?e ...]
         :where [_ :dustingetz/gender ?e]]
    *$*))

(defn gender []
  (d/q '[:find ?e .
         :where [_ :dustingetz/gender ?e]]
    *$*))

(tests
  (genders) => [17592186045418 17592186045419]
  (gender) => 17592186045418
  )

(defn shirt-sizes [gender]
  (d/q
    '[:in $ ?gender
      :find [?e ...]
      :where
      [?e :dustingetz/type :dustingetz/shirt-size]
      [?e :dustingetz/gender ?gender]]
    *$* gender))

(defn shirt-size [gender & [needle]]
  (d/q
    '[:in $ ?gender ?needle
      :find ?e .
      :where
      [?e :dustingetz/type :dustingetz/shirt-size]
      [?e :dustingetz/gender ?gender]
      [?e :db/ident ?ident]
      [(dustin.fiddle/needle-match ?ident ?needle)]]
    *$* gender (or needle "")))

(tests
  (shirt-sizes :dustingetz/male) => [17592186045421 17592186045422 17592186045423]
  (shirt-size :dustingetz/male) => 17592186045421
  (shirt-size :dustingetz/male "med") => 17592186045422
  (shirt-size :dustingetz/male "sm") => 17592186045421
  (d/touch (d/entity *$* 17592186045422))
  )

(defn needle-match [v needle]
  (clojure.string/includes?
    (.toLowerCase (or (str v) ""))
    (.toLowerCase (or (str needle) ""))))

(defn submission [needle]
  (d/q '[:find ?e . #_[?e ...]
         :in $ ?needle
         :where
         [?e :dustingetz/email ?email]
         [(dustin.fiddle/needle-match ?email ?needle)]]
       *$* needle))

(defn submissionS [needle]
  (d/q '[:find [?e ...]
         :in $ ?needle
         :where
         [?e :dustingetz/email ?email]
         [(dustin.fiddle/needle-match ?email ?needle)]]
       *$* needle))

(tests
  (submission "bob") => 17592186045429
  (submission "ali") => 17592186045428
  (submissionS "example")  => [17592186045428 17592186045429 17592186045430]
  )

;(def ast0 '[{(submission >needle) [{:dustingetz/gender
;                                    [:db/ident
;                                     {(shirt-size dustingetz/gender) [*]}]}]}
;            (gender)])
