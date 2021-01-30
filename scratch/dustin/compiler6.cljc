(ns dustin.compiler6
  (:require [minitest :refer [tests]]))

(declare expand <- )

;; Cardinality 1

[{(submission needle)
  [{:dustingetz/gender
    [{(shirt-size gender)
      [:db/id :db/ident]}]}]}]

(def hfql-expanded '{(quote (submission needle))
                     (let [% (fmapI submission needle)]
                       (let [% %]
                         {(quote :dustingetz/gender)
                          (let [% (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/gender) %)]
                            (let [gender %]
                              {(quote (shirt-size gender))
                               (let [% (fmapI shirt-size gender)]
                                 (let [% %]
                                   {(quote :db/id)    (fmapI (partial hyperfiddle.hfql20/hf-nav :db/id) %),
                                    (quote :db/ident) (fmapI (partial hyperfiddle.hfql20/hf-nav :db/ident) %)}))}))}))})

`{(quote (submission needle))
  (let [needle# (<- >needle)
        %# (submission needle#)]
    {(quote :dustingetz/gender)
     (let [%# (hf-nav :dustingetz/gender %#)
           gender# %#]
       {(quote (shirt-size gender#))
        (let [%# (shirt-size gender#)]
          {(quote :db/id)    (hf-nav :db/id %#),
           (quote :db/ident) (hf-nav :db/ident %#)})})})}

;; Cardinality many

(def hfql-ast '[{(submissions >needle) [:dustingetz/email
                                        {:dustingetz/gender
                                         [:db/ident
                                          {(shirt-sizes gender) [:db/ident]}]}]}
                {(genders) [:db/ident]}])

(def hfql-result '{(submissions needle) [#:dustingetz{:email  "alice@example.com",
                                                      :gender {:db/ident :dustingetz/female,
                                                               (shirt-sizes dustingetz/gender)
                                                               [#:db{:ident :dustingetz/womens-small}
                                                                #:db{:ident :dustingetz/womens-medium}
                                                                #:db{:ident :dustingetz/womens-large}]}}]})

[{'(submissions needle) (>> ;; splice
                         (let [needle#     (<- >needle)
                               submission# (<< :dustingetz/email (submissions needle#)) ;; split
                               ]
                           {(quote :dustingetz/email) (hf-nav :dustingetz/email submission#)
                            (quote :dustingetz/gender)
                            (let [gender# (hf-nav :dustingetz/gender submission#)]
                              {(quote :db/ident) (hf-nav :db/ident gender#)
                               (quote (shirt-size gender#))
                               (>>
                                (let [shirt-size# (<< :db/id (shirt-sizes gender#))]
                                  {(quote :db/id)    (hf-nav :db/id shirt-size#),
                                   (quote :db/ident) (hf-nav :db/ident shirt-size#)}))})}))}
 {'(genders) (>> (let [gender# (<< (genders))]
                   {(quote :db/ident) (hf-nav :db/ident gender#)}))}]


(defmacro rfor [[sym rcoll] & body]
  `(>> (let [~sym (<< (<- ~rcoll))] ~@body)))

{'(genders) (>> {:db/ident (hf-nav :db/ident (<< (genders)))})}

{'(genders) (rfor [gender (genders)]
                  {:db/ident (hf-nav :db/ident gender)})}


(def expand <<)
(def collapse >>)
(def diff <<)
(def patch >>)

(patch
 (let [submission# (diff :dustingetz/email (submissions (<- >needle)))]
   {:dustingetz/email (hf-nav :dustingetz/email submission#)
    :dustingetz/gender
    (let [gender# (hf-nav :dustingetz/gender submission#)]
      {:db/ident (hf-nav :db/ident gender#)
       '(shirt-size gender)
       (patch
        (let [shirt-size# (diff :db/id (shirt-sizes gender#))]
          {:db/id    (hf-nav :db/id shirt-size#),
           :db/ident (hf-nav :db/ident shirt-size#)}))})}))
