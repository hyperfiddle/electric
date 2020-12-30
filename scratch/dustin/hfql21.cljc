(ns dustin.hfql21
  "click the links"
  (:require
    [clojure.walk :refer [postwalk prewalk]]
    [minitest #?@(:clj [:refer [tests]])]
    #?(:clj [dustin.hfql20 :refer [hfql]])
    [hyperfiddle.incremental :as I
     :refer [sequenceI sequence-mapI extend-seqI
             bindI pureI fmapI capI joinI incr?]]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]])
  #?(:cljs
     (:require-macros
       [minitest :refer [tests]]
       [dustin.hfql20 :refer [hfql]])))

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

  (capI (I/sequence-some r))
  := '#:dustingetz{:gender #:db{:id 1, :ident :dustingetz/male}}

  (set! *1
    (let [a (pureI :dustingetz/male)]
      (hfql
        [{(shirt-sizes a) [:db/ident]}])))
  (-> *1 I/sequence-some capI I/sequence-some capI)
  := '{(shirt-sizes a) [#:db{:ident :dustingetz/mens-small}
                        #:db{:ident :dustingetz/mens-medium}
                        #:db{:ident :dustingetz/mens-large}]}

  (set! *1
    (let [a (pureI :dustingetz/male)]
      (hfql
        [{(shirt-sizes a) [:db/ident]}])))
  (-> *1 I/sequence-some (bindI I/sequence-some) capI)
  := '{(shirt-sizes a) [#:db{:ident :dustingetz/mens-small}
                        #:db{:ident :dustingetz/mens-medium}
                        #:db{:ident :dustingetz/mens-large}]}

  (set! *1
    (let [needle (pureI "")]
      (hfql
        [{(submission needle)
          [{:dustingetz/gender
            [{(shirt-sizes gender)
              [:db/id :db/ident]}]}]}])))

  (-> *1 I/sequence-some (bindI I/sequence-some) capI)
  := '{(submission needle)
       #:dustingetz{:gender {(shirt-sizes gender)
                             [#:db{:id 6, :ident :dustingetz/womens-small}
                              #:db{:id 7, :ident :dustingetz/womens-medium}
                              #:db{:id 8, :ident :dustingetz/womens-large}]}}}

  )
