(ns dustin.hf-nav
  (:require
    [datomic.api :as d]
    [hyperfiddle.api :as hf]
    [minitest :refer [tests]]))


(defn hf-nav [kf ref]
  (kf (d/entity hf/*$* ref)))  ; emits smart refs

(tests

  (hf-nav identity 17592186045440) => #:db{:id 17592186045440}
  (hf-nav :db/id 17592186045440) => 17592186045440
  (hf-nav :dustingetz/gender 17592186045441) => :dustingetz/male
  ;(hf-nav :dustingetz/gender 17592186045441) => #:db{:id 17592186045430}
  (d/entity hf/*$* 17592186045440) => #:db{:id 17592186045440}
  )
