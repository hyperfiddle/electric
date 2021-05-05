(ns dustin.fiddle-pages
  #?(:clj (:require [clojure.spec.alpha :as s]
                    [dustin.fiddle :refer [submissions genders shirt-sizes submission-details]]
                    [geoffrey.hfql.links :refer [hfql]]
                    [hfdl.lang :refer [dataflow]]
                    [hyperfiddle.api :as hf]))
  #?(:cljs (:require [clojure.spec.alpha :as s]
                     [hyperfiddle.api :as hf]
                     [dustin.fiddle])))

(s/fdef page-submissions :args (s/cat :needle string?))

(s/def :dustingetz/email string?)

#?(:clj
   (defn page-submissions [needle]
     (dataflow
      (hfql [{(submissions needle)
              [(:db/id ::hf/a (page-submission-details %))
               :dustingetz/email
               {:dustingetz/gender
                [:db/ident
                 {(shirt-sizes dustingetz/gender) [:db/ident]}]}]}
             {(genders) [:db/ident]}]))))

#?(:clj
   (defn page-submission-details [eid]
     (dataflow
      (hfql [{(submission-details eid) [:db/id
                                        :dustingetz/email
                                        :dustingetz/shirt-size
                                        {:dustingetz/gender [:db/ident {(shirt-sizes dustingetz/gender) [:db/ident]}]}]}
             {(genders) [:db/ident]}]))))
