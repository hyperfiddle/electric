(ns dustin.hfql.hfql-genders
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [user.gender-shirt-size :refer [genders shirt-sizes submissions]]))

    (p/defn page []
      (hf/hfql
        {(submissions "")
         [:dustingetz/email
          {(:dustingetz/gender & {::hf/options      (genders)
                                  ::hf/option-label :db/ident})
           [:db/ident]}
          {(:dustingetz/shirt-size & {::hf/options      (shirt-sizes gender "")
                                      ::hf/option-label :db/ident})
           [:db/ident]}]}))