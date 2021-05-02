(ns dustin.fiddle-pages
  (:require [dustin.fiddle :refer [submissions genders shirt-sizes submission-details]]
            [geoffrey.hfql.links :refer [hfql]]
            [hfdl.lang :refer [dataflow]]
            [hyperfiddle.api :as hf]))

(defn page-submissions [needle]
  (dataflow
   (hfql [{(submissions needle)
           [(:db/id ::hf/a (page-submission-details %))
            :dustingetz/email
            {:dustingetz/gender
             [:db/ident
              {(shirt-sizes dustingetz/gender) [:db/ident]}]}]}
          {(genders) [:db/ident]}])))


(defn page-submission-details [eid]
  (dataflow
   (hfql [{(submission-details eid) [:db/id
                                     :dustingetz/email
                                     :dustingetz/shirt-size
                                     {:dustingetz/gender [:db/ident {(shirt-sizes dustingetz/gender) [:db/ident]}]}]}
          {(genders) [:db/ident]}])))
