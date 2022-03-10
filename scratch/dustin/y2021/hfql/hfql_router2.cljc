(ns dustin.hfql-router2
  (:require
    [hyperfiddle.api :as hf]
    [hyperfiddle.photon :as p]
    [user.gender-shirt-size :refer [genders shirt-sizes submissions submission]]))

(def app
  (hf/app
    {((submissions .) ::hf/a (submission hf/tempid gender) ::hf/render button)
     [(:dustingetz/email ::hf/a (submission hf/e gender))
      :dustingetz/email1
      {(:dustingetz/gender ::hf/options (genders)) [:db/id :db/ident :dustingetz/type]}
      {(:dustingetz/shirt-size ::hf/options (shirt-sizes gender .)) [:db/ident]}]

     (shirt-sizes . .)
     [:db/ident]

     (submission .)
     [:db/id
      :dustingetz/email
      {(:dustingetz/gender ::hf/options (genders)) [:db/id :db/ident :dustingetz/type]}
      {(:dustingetz/shirt-size ::hf/options (shirt-sizes gender .)) [:db/ident]}]}))

(hf/main!
  'user.gender-shirt-size
  app)
