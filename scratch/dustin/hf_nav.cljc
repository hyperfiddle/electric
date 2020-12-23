(ns dustin.hf-nav
  (:require
    ;[datomic.api :as d]
    [datascript.core :as d]
    [dustin.dev :refer [*$*]]
    [minitest :refer [tests]]))


(defn hf-nav [kf ref]
  #_(println 'hf-nav kf ref)
  (kf (d/entity *$* ref)))  ; emits smart refs

(defn touch1 [ref]
  (d/pull *$* '[*] ref))

(comment
  ; datascript
  (hf-nav :db/ident 3) => :dustingetz/mens-small
  (hf-nav :db/id 3) => 3

  ; datascript
  (hf-nav identity [:dustingetz/email "alice@example.com"]) => #:db{:id 9}
  (hf-nav :db/id [:dustingetz/email "alice@example.com"]) => 9
  (hf-nav :dustingetz/gender [:dustingetz/email "alice@example.com"]) => :dustingetz/female

  ; datomic
  (hf-nav identity 17592186045429) :? (= *1 #:db{:id 17592186045429})
  (hf-nav :db/id 17592186045429) := 17592186045440
  (hf-nav :dustingetz/gender 17592186045429) := :dustingetz/male
  (d/entity *$* 17592186045429) :? (= (into {} *1) #:db{:id 17592186045429})
  (into {} (d/entity *$* 17592186045429))
  )
