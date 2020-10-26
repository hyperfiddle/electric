(ns user.dustin.hfql
  (:require
    [datomic.api :as d]
    [backtick :refer [template]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.config]
    [hyperfiddle.api :as hf]
    ))

; /:user.hello-world!submission-detail/~entity('$',(:dustingetz!email,'alice@example.com'))
(def args [[:dustingetz/email "alice@example.com"]])
(defn submission-detail [e]
  (d/pull e (hf/get-db "$")
    [:db/id
     :dustingetz/email
     {:dustingetz/gender [:db/ident
                          `shirt-sizes]}
     {:dustingetz/shirt-size [:db/ident]}
     `genders]))

(defn genders [needle2]
  (d/q '[:find [(pull ?e [:db/ident]) ...]
         :where [?e :scratch/type :scratch/gender]]
    (hf/get-db "$")))

(defn shirt-sizes [gender needle]
  (d/q
    [:in $ ?gender
     :find [?e ...]
     :where
     [?e :scratch/type :scratch/shirt-size]
     [?e :scratch/gender ?gender]]
    (hf/get-db "$") gender))

(defn submissions [needle]
  (d/q '[:find [?e ...]
         :in $ ?needle
         :where
         [?e :dustingetz/email ?needle]]
    (hf/get-db "$")
    needle))

; /:user.hello-world!submission-master/needle
(def submission-master [needle]
  [{`(submissions ~needle) [:dustingetz/email
                            {:dustingetz/gender [:db/ident
                                                 `(shirt-sizes)
                                                 `(->> (shirt-sizes))
                                                 `(shirt-sizes ~needle)]}
                            {:dustingetz/shirt-size [:db/ident]}]}
   `(genders)])

(defmethod hf/render `genders [ctx props])
(defmethod hf/render `shirt-sizes [ctx props])

(defmethod hf/render :dustingetz/gender [ctx props]
  [select ctx {:options `genders}])

(defmethod hf/render :dustingetz/shirt-sizes [ctx props]
  [select ctx {:options `shirt-sizes}])

(comment
  {:fiddle/ident :user.hello-world/submission-detail,
   :fiddle/links
   [{:link/path ":user.hello-world/submission-detail"
     :link/class [:hf/iframe],
     :link/fiddle
     {:fiddle/ident :user.hello-world/genders,
      :fiddle/query "[:find\n (pull ?e [:db/ident])\n :where\n [?e :db/ident ?i]\n [(namespace ?i) ?ns]\n [(ground \"dustingetz.gender\") ?ns]]",
      :fiddle/type :query}}
    {:link/path ":dustingetz/gender"
     :link/class [:hf/iframe],
     :link/fiddle
     {:fiddle/ident :user.hello-world/shirt-sizes,
      :fiddle/query "[:find\n (pull ?e [:db/ident])\n :in $ ?gender\n :where\n [?e :db/ident ?i]\n [?e :dustingetz.reg/gender ?gender]\n [(namespace ?i) ?ns]\n [(ground \"dustingetz.shirt-size\") ?ns]]",
      :fiddle/type :query}}],
   :fiddle/pull "[:db/id\n :dustingetz/email\n {:dustingetz/gender [:db/ident]}\n {:dustingetz/shirt-size [:db/ident]}]",
   :fiddle/type :entity}
  )
