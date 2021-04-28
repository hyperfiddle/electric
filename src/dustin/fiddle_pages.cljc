(ns dustin.fiddle-pages
  (:require [dustin.fiddle :refer [submissions genders shirt-sizes submission-details]]
            [geoffrey.hfql.links :refer [hfql]]
            [hfdl.lang :refer [dataflow]]
            [hyperfiddle.api :as hf]))

(defn page-submissions [needle]
  (dataflow
   (let [needle needle]
     (hfql [{(submissions needle)
             [(:db/id ::hf/a (submission-details %))
              :dustingetz/email
              {:dustingetz/gender
               [:db/ident
                {(shirt-sizes dustingetz/gender) [:db/ident]}]}]}
            {(genders) [:db/ident]}]))))
