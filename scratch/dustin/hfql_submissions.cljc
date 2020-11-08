(ns dustin.hfql-submissions)

(defn genders [])
(defn shirt-sizes [gender])
(defn submissions [needle1 needle2])

(defn ^:hyperfiddle.api/fiddle submission-master [needle]
  [{`(submissions ~needle) [:dustingetz/email
                            {:dustingetz/gender [:db/ident
                                                 ;`(shirt-sizes)
                                                 {`(shirt-sizes) [:db/id
                                                                  `(genders) [:db/ident]]}]}
                            {:dustingetz/shirt-size [:db/ident]}]}
   {`(genders) [:db/ident]}])

(->> (eval `(genders))
  (d/pull-many pp))

; `(genders)
; `(submission-master ~needle)
; `(shirt-sizes :scratch/male1)

; page load
; iframe update

; universal form, normalized
; EDN control plane, an operational UI
{`(genders [:db/ident])
 [#:db{:ident :scratch/male}
  #:db{:ident :scratch/female}]

 `(shirt-sizes :scratch/male [:db/ident
                              {`(genders) [:db/ident]}])
 [#:db{:ident :scratch/mens-small}
  #:db{:ident :scratch/mens-medium}
  #:db{:ident   :scratch/mens-large
       'genders ~`(genders [:db/ident])}]

 `(shirt-sizes :scratch/female)
 [#:db{:ident :scratch/womens-medium}
  #:db{:ident :scratch/womens-large}
  #:db{:ident :scratch/womens-small}]

 `(submissions 42 "example")                                             ; `(submissions since)
 [#:scratch{:email  "alice@example.com",
            :gender {:db/ident    :scratch/male1,
                     'shirt-sizes ~`(shirt-sizes :scratch/male1)}}
  #:scratch{:email  "bob@example.com",
            :gender {:db/ident    :scratch/male,
                     'shirt-sizes ~`(shirt-sizes :scratch/male)}}
  #:scratch{:email  "charlie@example.com",
            :gender {:db/ident    :scratch/male,
                     'shirt-sizes ~`(shirt-sizes :scratch/male)}}]}


;`(submissions 42 "example")
;[#:scratch{:email  "alice@example.com",
;           :gender {:db/ident    :scratch/female,
;                    'shirt-sizes [#:db{:ident :scratch/womens-medium}
;                                  #:db{:ident :scratch/womens-large}
;                                  #:db{:ident :scratch/womens-small}]}}]
