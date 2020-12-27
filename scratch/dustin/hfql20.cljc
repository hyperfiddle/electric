(ns dustin.hfql20
  #?(:cljs (:require-macros [minitest :refer [tests]]))
  (:require
    [hyperfiddle.hfql19 :refer [sequenceI bindI pureI fmapI capI
                                hf-nav]]
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

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol (name edge)) #_edge '%))

(defn compile-hfql*
  "compile HFQL form to s-expressions in Incremental"
  [form]
  (m/match form

    [!pats ...]
    (apply merge (mapv compile-hfql* !pats))

    {& (m/seqable [?edge ?cont])}
    ; let is for in cardinality many
    `(~'let [~'% ~(get (compile-hfql* ?edge) ?edge)
           ~(hf-edge->sym ?edge) ~'%]
       {'~?edge ~(compile-hfql* ?cont)})

    ?form
    (cond
      (keyword? ?form)
      `{~?form (~'fmapI (~'partial ~'hf-nav ~?form) ~'%)}

      (seq? ?form)
      (let [[f & args] ?form]
        `{~?form (~'fmapI ~f ~@args)}))))

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

  (def % (pureI bob))
  (compile-hfql* ':dustingetz/gender)
  (eval *1)
  (capI *1) := :dustingetz/male

  (compile-hfql* [:dustingetz/gender :db/id])
  (eval *1)
  (capI (sequenceI (vals *1))) :=  [:dustingetz/male 10]

  (compile-hfql* {:dustingetz/gender [:db/id :db/ident]})
  (eval *1)
  (capI (sequenceI *1)) := [1 :dustingetz/male]
  ;:= #:dustingetz{:gender #:db{:ident :dustingetz/male, :id male}}
  (def needle (pureI ""))

  (compile-hfql* '[{(submission needle)
                    [{:dustingetz/gender
                      [{(shirt-size gender)
                        [:db/id :db/ident]}]}]}])

  := (let [% (fmapI submission needle) % %]
       {(quote (submission needle))
        (let [% (fmapI (partial hf-nav :dustingetz/gender) %) gender %]
          {(quote :dustingetz/gender)
           (let [% (fmapI shirt-size gender) % %]
             {(quote (shirt-size gender))
              #:db{:id (fmapI (partial hf-nav :db/id) %),
                   :ident (fmapI (partial hf-nav :db/ident) %)}})})})

  (eval *1)
  := '{(submission needle)
       #:dustingetz{:gender
                    {(shirt-size gender)
                     #:db{:id '..., :ident '...}}}}
  (def x *1)
  (-> x
    (get '(submission needle))
    :dustingetz/gender
    (get '(shirt-size gender))
    :db/ident
    capI)
  := :dustingetz/womens-small

  ; if something is wrapped in a stream
  ; R2D2 needs to resolve it
  ; he can choose not to resolve it
  ; this is link following ... binding to a stream is link traversal
  ; the client is in control of this
  ; the client adjusts the computation
  ; the server just says, these streams are available

  ; cardinality will push the structure harder
  ; "how many shirt-sizes" is inside an I
  ; unclear how to join the streams nicely into final result
  ; but do we even need final result?

  ;broken
  (compile-hfql* '{(:dustingetz/gender %) [(:db/id %) (:db/ident %)]})
  (eval *1)
  (capI (sequenceI *1))
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
