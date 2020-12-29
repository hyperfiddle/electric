(ns dustin.hfql20
  #?(:cljs (:require-macros [minitest :refer [tests]]))
  (:require
    [hyperfiddle.hfql19 :refer [sequenceI sequence-mapI bindI pureI fmapI capI joinI
                                hf-nav]]
    [meander.epsilon :as m]
    [minitest #?@(:clj [:refer [tests]])]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]
    #?(:clj [lexikon.core :refer [lexical-eval]])))

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

(def cardinality* {'submissions :db.cardinality/many
                   'shirt-sizes :db.cardinality/many})

(defn cardinality [form]
  (cond
    (symbol? form) (get cardinality* form)
    (keyword? form) (get cardinality* form)
    (seq? form) (let [[f & args] form]
                  (get cardinality* f)
                  #_(:db/cardinality (meta f)))))

(tests
  ;(cardinality `(shirt-sizes _ _)) := nil
  (cardinality `shirt-sizes) := :db.cardinality/many)

(defn many? [form] (= :db.cardinality/many (cardinality form)))

(tests
  (many? :dustingetz/gender) := false
  (many? 'shirt-sizes) := true
  (many? '(shirt-sizes a)) := true
  (many? '(shirt-size a)) := true
  (many? 'shirt-size) := false)

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol (name edge)) #_edge '%))

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form)
    `(~'fmapI (~'partial ~'hf-nav ~?form) ~'%)

    (seq? ?form)
    (let [[f & args] ?form]
      `(~'fmapI ~f ~@args))))

(defn compile-hfql*
  "compile HFQL form to s-expressions in Incremental"
  [form]
  (m/match form

    [!pats ...]
    (apply merge (mapv compile-hfql* !pats))

    {& (m/seqable [?edge ?cont])}
    (let [edge* (compile-leaf* ?edge)]
      (if (many? ?edge)                                     ; thus % is sequential, (pureI [1 2 3])
        `{'~?edge
          (~'fmapI (~'fn [~'>as]
                     (~'for [~'% ~'>as]
                       (~'let [~(hf-edge->sym ?edge) ~'%]
                         ~(compile-hfql* ?cont))))
            (~'unsequenceI ~edge*))}                        ; extend monad
        `{'~?edge
          (~'let [~'% ~edge*]
            (~'let [~(hf-edge->sym ?edge) ~'%]
              ~(compile-hfql* ?cont)))}))

    ?form
    `{'~?form ~(compile-leaf* ?form)}))

(tests
  "cardinality many"
  (shirt-sizes :dustingetz/male) := [3 4 5]
  (many? '(shirt-sizes a)) := true

  (compile-hfql* '[{(shirt-size a) [:db/ident]}])
  (compile-hfql* '[{(shirt-sizes a) [:db/ident]}])
  (lexical-eval {'a (pureI :dustingetz/male)} *1)
  (-> *1 (get '(shirt-sizes a)))
  ; each cardinality strata in the pull extends the dimension by 1
  ; we must joinI for each extension here
  (capI (joinI (fmapI sequenceI (fmapI #(map :db/ident %) *1))))
  := [:dustingetz/mens-small
      :dustingetz/mens-medium
      :dustingetz/mens-large]
  ; the goal of above is to produce the identity structure, no streams
  ; without removing levels of the map

  ;(capI (unsequenceI (pureI [1 2 3])))
  ;(capI (sequenceI *1))

  (compile-hfql* '[{(submissions a)
                    [{:dustingetz/gender
                      [{(shirt-sizes gender)
                        [:db/id :db/ident]}]}]}])
  (lexical-eval {'a (pureI "")} *1)
  (def x *1)
  (-> x
    (get '(submissions a))
    (->> (fmapI (fn [a]
                  (println a)
                  (sequenceI
                    (map
                      (comp
                        sequence-mapI
                        #(get % '(shirt-sizes gender))
                        :dustingetz/gender)
                      a))))
      joinI)
    capI
    #_sequenceMapI
    #_capI
    )
  )

(tests
  (compile-hfql* '(identity a))
  := '{(quote (identity a)) (fmapI identity a)}
  (let [a (pureI 1)] (lexical-eval *1)) := '{(identity >needle) _}
  (-> *1 (get '(identity a)) capI) := 1

  (compile-hfql* '{(identity >needle) (inc %)})
  := '(let [% (fmapI identity >needle) % %]
        {(quote (identity >needle)) (fmapI inc %)})

  (compile-hfql*
    '{(identity a)
      [(dec %)
       (inc %)]})
  (lexical-eval {'a (pureI 1)} *1)
  := '{(identity >needle) {(dec %) '...,
                           (inc %) '...}}
  ; R2D2 traverse structure, click on links
  ;(capI *1) := 1

  (compile-hfql* '{(inc a) (inc %)})
  := '(let [% (fmapI inc a) % %]
        {(quote (inc a)) (fmapI inc %)})
  (lexical-eval {'a (pureI 1)} *1)
  (-> *1 (get '(inc a)) (get '(inc %)) capI) := 3

  (hf-nav :dustingetz/gender bob) := :dustingetz/male
  (compile-hfql* :dustingetz/gender)
  := '#:dustingetz{:gender (fmapI (partial hf-nav :dustingetz/gender) %)}
  (lexical-eval {'% (pureI bob)} *1) := '{:dustingetz/gender _}
  (capI (:dustingetz/gender *1)) := :dustingetz/male

  (shirt-size :dustingetz/male) := 3
  (compile-hfql* '(shirt-size %))
  := '{(quote (shirt-size %)) (fmapI shirt-size %)}
  (let [% (pureI :dustingetz/male)] (lexical-eval *1)) := '{(shirt-size %) _}
  (-> *1 (get '(shirt-size %)) capI) := 3

  (compile-hfql* [:dustingetz/gender :db/id])
  := '{(quote :dustingetz/gender) (fmapI (partial hf-nav :dustingetz/gender) %),
       (quote :db/id)             (fmapI (partial hf-nav :db/id) %)}
  (lexical-eval {'% (pureI bob)} *1) := '{:dustingetz/gender _, :db/id _}
  (capI (:dustingetz/gender *1)) := :dustingetz/male

  (compile-hfql* {:dustingetz/gender [:db/id :db/ident]})
  := '(let [% (fmapI (partial hf-nav :dustingetz/gender) %)
            gender %]
        {(quote :dustingetz/gender)
         {(quote :db/id)    (fmapI (partial hf-nav :db/id) %),
          (quote :db/ident) (fmapI (partial hf-nav :db/ident) %)}})
  (lexical-eval {'% (pureI bob)} *1) := '#:dustingetz{:gender #:db{:id _, :ident _}}
  (capI (sequenceI (vals (select-keys (:dustingetz/gender *1) [:db/ident :db/id])))) := [:dustingetz/male 1]

  (compile-hfql* [{:dustingetz/gender [:db/id :db/ident]}])
  := '(let [% (fmapI (partial hf-nav :dustingetz/gender) %)
            gender %]
        {(quote :dustingetz/gender)
         #:db{:id    (fmapI (partial hf-nav :db/id) %),
              :ident (fmapI (partial hf-nav :db/ident) %)}})
  (lexical-eval {'% (pureI bob)} *1) := '#:dustingetz{:gender #:db{:id _, :ident _}}
  (capI (sequenceI (vals (select-keys (:dustingetz/gender *1) [:db/ident :db/id])))) := [:dustingetz/male 1]

  (compile-hfql* '[{:dustingetz/gender [{(shirt-size gender) [:db/id :db/ident]}]}])
  := '(let [% (fmapI (partial hf-nav :dustingetz/gender) %)
            gender %]
        {(quote :dustingetz/gender)
         (let [% (fmapI shirt-size gender) % %]
           {(quote (shirt-size gender))
            #:db{:id    (fmapI (partial hf-nav :db/id) %),
                 :ident (fmapI (partial hf-nav :db/ident) %)}})})
  (lexical-eval {'% (pureI bob)} *1)
  := '#:dustingetz{:gender {(shirt-size gender) #:db{:id _, :ident _}}}
  (capI (sequenceI (vals (select-keys (-> *1 :dustingetz/gender (get '(shirt-size gender))) [:db/ident :db/id]))))
  := [:dustingetz/mens-small 3]

  (compile-hfql* '[{(submission needle)
                    [{:dustingetz/gender
                      [{(shirt-size gender)
                        [:db/id :db/ident]}]}]}])
  := '(let [% (fmapI submission needle) % %]
        {(quote (submission needle))
         (let [% (fmapI (partial hf-nav :dustingetz/gender) %) gender %]
           {(quote :dustingetz/gender)
            (let [% (fmapI shirt-size gender) % %]
              {(quote (shirt-size gender))
               #:db{:id    (fmapI (partial hf-nav :db/id) %),
                    :ident (fmapI (partial hf-nav :db/ident) %)}})})})
  (lexical-eval {'needle (pureI "alice")} *1) := '{(submission needle) #:dustingetz{:gender {(shirt-size gender) #:db{:id _, :ident _}}}}
  (-> *1 (get '(submission needle)) :dustingetz/gender
    (get '(shirt-size gender)) :db/ident capI) := :dustingetz/womens-small

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

  (compile-hfql* '{:dustingetz/gender [:db/id (:db/ident %)]})
  := '(let [% (fmapI (partial hf-nav :dustingetz/gender) %) gender %]
        {(quote :dustingetz/gender)
         {:db/id        (fmapI (partial hf-nav :db/id) %),
          (:db/ident %) (fmapI :db/ident %)}})

  (compile-hfql* '{(:dustingetz/gender %) [(:db/id %) (:db/ident %)]})
  := '(let [% (fmapI :dustingetz/gender %) % %]
        {(quote (:dustingetz/gender %))
         {(:db/id %)    (fmapI :db/id %),
          (:db/ident %) (fmapI :db/ident %)}})

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
