(ns dustin.hfql20
  #?(:cljs (:require-macros [minitest :refer [tests]]))
  (:require
    [hyperfiddle.hfql :refer [sequenceI bindI pureI fmapI capI]]
    [meander.epsilon :as m]
    [minitest #?@(:clj [:refer [tests]])]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]))

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

; write the hfql compiler

(def cardinality {`submissions :db.cardinality/many
                  `shirt-sizes :db.cardinality/many})

(defn compile-hfql*
  "compile HFQL form to s-expressions in Incremental"
  [form]
  (m/match form

    [!pats ...]
    #_(apply merge) (mapv compile-hfql* !pats)

    {& (m/seqable [?edge ?cont])}
    `(let [~'% #_(get '~?edge) ~(compile-hfql* ?edge)]
       #_{~?edge ~(compile-hfql* ?cont)}
       ~(compile-hfql* ?cont))

    ?form
    (let [[f & args] ?form]
      #_`{'~?form (fmapI ~f ~@args)}
      `(fmapI ~f ~@args))))

(tests
  (def >needle (pureI 1))
  (capI (eval (compile-hfql* '(identity >needle)))) := 1
  #_{(identity >needle) >as...}

  (compile-hfql*
    '{(identity >needle)
      (inc %)})
  := '(clojure.core/let [% (hyperfiddle.hfql19/fmapI identity >needle)]
        (hyperfiddle.hfql19/fmapI inc %))

  (compile-hfql*
    '{(identity >needle)
      [(dec %)
       (inc %)]})
  (eval *1)
  (capI *1) := 1

  (compile-hfql* '{(inc >needle) (inc %)})
  := '(clojure.core/let [% (hyperfiddle.hfql19/fmapI inc >needle)]
        (hyperfiddle.hfql19/fmapI inc %))

  (compile-hfql* '{(inc >needle) (inc %)})
  (eval *1)
  (capI *1) := 3

  )

(defmacro compile-hfql [form]
  (compile-hfql* form))

(tests
  (capI (compile-hfql (identity >needle))) := 1
  (capI (compile-hfql {(identity >needle) (identity %)})) := 1
  (capI (compile-hfql {(inc >needle) (inc %)})) := 3
  (capI (compile-hfql {(inc >needle) {(inc %) (inc %)}})) := 4

  ; the macro compiles HFQL to sexprs in Incremental
  (macroexpand-1 '(compile-hfql (identity >needle)))
  := (hyperfiddle.hfql/fmapI identity >needle)

  (capI (compile-hfql (identity >needle))) := 1


  (macroexpand-1 '(compile-hfql {(identity >needle) (identity %)}))
  := '(clojure.core/let [% (dustin.hfql20/compile-hfql (identity >needle))]
        (dustin.hfql20/compile-hfql (identity %)))



  ;:= #:dustingetz{:gender :dustingetz/male}

  )
