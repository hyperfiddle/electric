(ns dustin.y2022.destructure-pull
  (:require [contrib.data :refer [unqualify]]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]))

;(defn destructure-pull [])

(tests

  ; "flat" tree with no
  (def tree {:db/valueType {:db/ident :db.type/ref}
             :db/cardinality {:db/ident :db.cardinality/one}})

  (let [{:keys [:db/valueType :db/cardinality]} tree]
    [(unqualify (:db/ident valueType))
     (unqualify (:db/ident cardinality))])
  := [:ref :one]

  ((juxt (comp unqualify :db/ident :db/valueType)
         (comp unqualify :db/ident :db/cardinality)) tree)
  := [:ref :one]

  (let [{:keys [{:db/valueType {:db/ident unqualify}}
                {:db/cardinality {:db/ident unqualify}}]} tree]
    [valueType cardinality])


  (let [{:keys [{:db/valueType {:db/ident unqualify}}
                {:db/cardinality {:db/ident unqualify}}]} tree
        [valueType cardinality]])


  (let [{:keys [:db/valueType :db/cardinality]} (a schema)
        x [(unqualify (:db/ident valueType))
           (unqualify (:db/ident cardinality))]]
    (case x
      [:ref :one] (into (sorted-map) v) ; todo lift sort to the pull object
      ;[:ref :many] v ; can't sort, no sort key
      nil))

  )
