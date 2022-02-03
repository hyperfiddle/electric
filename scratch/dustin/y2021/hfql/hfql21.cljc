(ns dustin.hfql21
  "click the links"
  (:require
    [clojure.walk :refer [postwalk prewalk]]
    [minitest #?@(:clj [:refer [tests]])]
    #?(:clj [hyperfiddle.hfql20 :refer [hfql]])
    [hyperfiddle.incremental :as I
     :refer [sequenceI sequence-mapI extend-seq
             bindI pureI fmapI capI joinI incr?]]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]])
  #?(:cljs
     (:require-macros
       [minitest :refer [tests]]
       [hyperfiddle.hfql20 :refer [hfql]])))

; The server serves an infinite dag.
; The served response is a continuous flow, extended infinite layers.
; The caller is responsible for choosing how deep to sample.
; This is hypermedia link browsing.

(tests
  (def r
    (let [% (pureI bob)]
      (hfql {:dustingetz/gender [:db/id :db/ident]})))

  (I/scan incr? r)
  := [[:dustingetz/gender :db/id] [:dustingetz/gender :db/ident]]

  (capI (I/sequence-some r incr?))
  := '#:dustingetz{:gender #:db{:id 1, :ident :dustingetz/male}}

  (let [a (pureI :dustingetz/male)]
    (hfql
      [{(shirt-sizes a) [:db/ident]}]))
  (-> *1 I/sequence-some capI I/sequence-some capI)
  := '{(shirt-sizes a) [#:db{:ident :dustingetz/mens-small}
                        #:db{:ident :dustingetz/mens-medium}
                        #:db{:ident :dustingetz/mens-large}]}

  (let [a (pureI :dustingetz/male)]
    (hfql
      [{(shirt-sizes a) [:db/ident]}]))
  (-> *1 I/sequence-some (bindI I/sequence-some) capI)
  := '{(shirt-sizes a) [#:db{:ident :dustingetz/mens-small}
                        #:db{:ident :dustingetz/mens-medium}
                        #:db{:ident :dustingetz/mens-large}]}

  (let [a (pureI "")]
    (hfql
      [{(submissions a)
        [{:dustingetz/gender
          [{(shirt-sizes gender)
            [:db/id :db/ident]}]}]}]))

  (-> *1
    I/sequence-some
    (bindI I/sequence-some)
    (bindI I/sequence-some)
    capI)
  := '{(submissions a)
       [{:dustingetz/gender
         {(shirt-sizes gender)
          [#:db{:id 6, :ident :dustingetz/womens-small}
           #:db{:id 7, :ident :dustingetz/womens-medium}
           #:db{:id 8, :ident :dustingetz/womens-large}]}}
        {:dustingetz/gender
         {(shirt-sizes gender)
          [#:db{:id 3, :ident :dustingetz/mens-small}
           #:db{:id 4, :ident :dustingetz/mens-medium}
           #:db{:id 5, :ident :dustingetz/mens-large}]}}
        {:dustingetz/gender
         {(shirt-sizes gender)
          [#:db{:id 3, :ident :dustingetz/mens-small}
           #:db{:id 4, :ident :dustingetz/mens-medium}
           #:db{:id 5, :ident :dustingetz/mens-large}]}}]}

  (let [a (pureI "")]
    (hfql [{(submissions a)
            [{:dustingetz/gender
              [{(shirt-sizes gender)
                [:db/id :db/ident]}]}
             :dustingetz/email]}]))
  ;:= '{(submissions a) _}

  (as-> *1 %
    (I/sequence-at % [['(submissions a)]])
    (bindI % #(I/sequence-at % [['(submissions a) 0 :dustingetz/email]
                                ['(submissions a) 0 :dustingetz/gender '(shirt-sizes gender)]
                                ['(submissions a) 1 :dustingetz/email]
                                ['(submissions a) 1 :dustingetz/gender '(shirt-sizes gender)]]))
    (bindI % #(I/sequence-at % [['(submissions a) 0 :dustingetz/gender '(shirt-sizes gender) 0 :db/id]
                                ['(submissions a) 0 :dustingetz/gender '(shirt-sizes gender) 0 :db/ident]
                                ['(submissions a) 0 :dustingetz/gender '(shirt-sizes gender) 1 :db/id]
                                ['(submissions a) 0 :dustingetz/gender '(shirt-sizes gender) 1 :db/ident]
                                ['(submissions a) 0 :dustingetz/gender '(shirt-sizes gender) 2 :db/id]
                                ['(submissions a) 0 :dustingetz/gender '(shirt-sizes gender) 2 :db/ident]
                                ['(submissions a) 1 :dustingetz/gender '(shirt-sizes gender) 0 :db/id]
                                ['(submissions a) 1 :dustingetz/gender '(shirt-sizes gender) 0 :db/ident]
                                ['(submissions a) 1 :dustingetz/gender '(shirt-sizes gender) 1 :db/id]
                                ['(submissions a) 1 :dustingetz/gender '(shirt-sizes gender) 1 :db/ident]
                                ['(submissions a) 1 :dustingetz/gender '(shirt-sizes gender) 2 :db/id]
                                ['(submissions a) 1 :dustingetz/gender '(shirt-sizes gender) 2 :db/ident]]))
    (capI %))

  )
