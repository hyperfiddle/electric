(ns dustin.fiddle
  (:require
    [datomic.api :as d]
    [hyperfiddle.api :as hf]
    [minitest :refer [tests]]))


(defn genders []
  (d/q '[:find [?e ...]
         :where [_ :dustingetz/gender ?e]]
    hf/*$*))

(defn gender []
  (d/q '[:find ?e .
         :where [_ :dustingetz/gender ?e]]
    hf/*$*))

(tests
  (genders) => [17592186045430 17592186045431]
  (gender) => 17592186045430
  )

;(defn shirt-sizes [gender #_needle]
;  (d/q
;    '[:in $ ?gender
;      :find [?e ...]
;      :where
;      [?e :scratch/type :scratch/shirt-size]
;      [?e :scratch/gender ?gender]]
;    hf/*$* gender))
;
(defn shirt-size [gender #_needle]
  (d/q
    '[:in $ ?gender
      :find ?e .
      :where
      [?e :dustingetz/type :dustingetz/shirt-size]
      [?e :dustingetz/gender ?gender]]
    hf/*$* gender))

(tests
  ;(shirt-sizes 17592186045430) => [17592186045430 17592186045431]
  (shirt-size 17592186045430) => 17592186045433
  (shirt-size :dustingetz/male) => 17592186045433
  )

(defn submission [needle]
  ;{:pre [(doto needle (println 'submission))]}
  (d/q '[:find ?e . #_[?e ...]
         :in $ ?needle
         :where
         [?e :dustingetz/email ?email]
         [(hyperfiddle.api/needle-match ?email ?needle)]]
    hf/*$* needle))

(tests
  (submission "bob") => 17592186045441
  (submission "ali") => 17592186045440
  )

;(def ast0 '[{(submission >needle) [{:dustingetz/gender
;                                    [:db/ident
;                                     {(shirt-size dustingetz/gender >needle2) [*]}]}]}
;            (gender)])
