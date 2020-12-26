(ns dustin.hfql20
  #?(:cljs (:require-macros [minitest :refer [tests]]))
  (:require
    [hyperfiddle.hfql :refer [sequenceI bindI pureI fmapI capI]]
    [minitest #?@(:clj [:refer [tests]])]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]))

; How does cardinality work?
; HFQL today - if it returns sequential?, do cardinality things
; Datomic pull - attributes are marked cardinality/many
;
; Cardinality is known at compile time !

'[{(submissions needle)
   [{:dustingetz/gender
     [:db/ident
      {(shirt-sizes dustingetz/gender needle:dustingetz/email)
       [:db/ident]}]}
    :dustingetz/email]}]

(tests
  (def >needle (pureI ""))
  (capI (fmapI submissions >needle)) := [9 10 11]
  (capI (sequenceI [(pureI 9) (pureI 10) (pureI 11)])) := [9 10 11]
  (capI (bindI (fmapI submissions >needle) pureI)) := [9 10 11]
  )

; unsequenceI :: I Seq a -> I Seq I a -- reactive list with reactive elements
(defn unsequenceI
  "this is not quite the opposide of sequence, it extends a layer"
  [>as]
  (->> >as
    (fmapI (fn [as]
             ; allocate inputs and introduce layer
             (map pureI as)))))

(tests
  (def >>as (unsequenceI (pureI [9 10 11])))
  (map capI (capI >>as)) := [9 10 11]
  (capI (bindI >>as sequenceI)) := [9 10 11]

  (capI (bindI (unsequenceI (pureI [1 2 3])) sequenceI)) := [1 2 3]
  (capI (capI (fmapI sequenceI (unsequenceI (pureI [1 2 3]))))) := [1 2 3]

  (->> >>as
    (fmapI (fn [>as]
             (->> >as
               (map (fn [>a]
                      (fmapI identity #_:dustingetz/email >a)))))))
  (capI (bindI *1 sequenceI)) := [9 10 11]
  )

(comment

  (for [>a ~(submissions ~>needle)]
    (let [>dustingetz/email (:dustingetz/email ~>a)
          >dustingetz/gender (:dustingetz/gender ~>a)
          >db/ident (:db/ident ~>dustingetz/gender)]
      (for [>a ~(shirt-sizes ~>dustingetz/gender ~>dustingetz/email)]
        (let [>db/ident (:db/ident ~>a)]))))

  ; compiler macro produces this, given static cardinalities:
  (bindI (unsequenceI (fmapI submissions >needle)) (fn [>as]
    (->> >as (map (fn [>a]
      (let [>dustingetz/email (fmapI :dustingetz/email >a)
            >dustingetz/gender (fmapI :dustingetz/gender >a)
            >db/ident (fmapI :db/ident >dustingetz/gender)]
        (bindI (unsequenceI (fmapI shirt-sizes >dustingetz/gender >dustingetz/email)) (fn [>as]
          (->> >as (map (fn [>a]
            (let [>db/ident (fmapI :db/ident >a)]))))))))))))

  ; bind ... unsequence ... map
  ; is the essence of IncrementalSeq ?
  )
