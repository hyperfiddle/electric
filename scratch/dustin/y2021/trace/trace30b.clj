(ns dustin.trace30b
  (:require
    [minitest :refer [tests]]
    [hyperfiddle.incremental :as I
     :refer [sequenceI sequence-mapI
             bindI pureI fmapI capI joinI incr?]]
    [hyperfiddle.hfql20 :refer [hfql]]
    [leo.extend-seq :refer [extend-seq]]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]))


(declare rfor ...)

(comment
  (macroexpand-1
    '(hfql [{(submissions >needle) [:dustingetz/email :db/id]}]))

  ; In HFQL, the notion of `{'~?edge ...}  is actually a render-edn concern
  ; The tree-nesting in HFQL is an accident, that's presentation

  )

(comment
  (def >as (submissions >needle))

  ; render-table
  (def >table [:table [:span (count ~>as)]
               (rfor :db/id [>a ~>as]
                 [:tr [:td (hf-nav :dustingetz/email ~>a)]])])
  (capI >table)
  := [:table [:span 3] [:tr "alice"] [:tr "bob"] [:tr "charlie"]]

  ; render-edn
  (def >edn {'(submissions >needle)
             (vec (rfor :db/id [>a ~>as]
                    {:dustingetz/email (hf-nav :dustingetz/email ~>a)
                     :db/id            (hf-nav :dustingetz/email ~>a)}))})

  (capI >edn)
  := {'(submissions >needle)
      [{:dustingetz/email "alice" :db/id 9}
       {:dustingetz/email "bob" :db/id 10}
       {:dustingetz/email "charlie" :db/id 11}]}

  )

#?(:clj (defn hf-nav [kf e] ...))

(defn render-record [visit-header visit-row >as]
  (visit-header ~>as)
  (rfor :db/id [>a ~>as]
    (visit-row {:dustingetz/email (hf-nav :dustingetz/email ~>a)
                :db/id            (hf-nav :dustingetz/email ~>a)})))

(defn render-edn [>as]
  (render-record
    (fn [as] {'(submissions >needle) as})
    (fn [a] a)
    >as))

(defn render-table [>as]
  (render-record
    (fn [as] [:table [:span (count as)]])
    (fn [a] [:tr (:dustingetz/email a)])
    >as))
