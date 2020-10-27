(ns dustin.hf-nav
  (:require
    [datomic.api :as d]
    [dustin.dev :refer [*$*]]
    [minitest :refer [tests]]))


(defn hf-nav [kf ref]
  (kf (d/entity *$* ref)))  ; emits smart refs

(tests

  (hf-nav identity 17592186045429) => #:db{:id 17592186045429}
  (hf-nav :db/id 17592186045429) => 17592186045440
  (hf-nav :dustingetz/gender 17592186045429) => :dustingetz/male
  (d/entity *$* 17592186045429) => #:db{:id 17592186045429}
  )
