(ns dustin.hfql22
  "cardinality strata"
  #?(:cljs (:require-macros [minitest :refer [tests]]
                            [hyperfiddle.hfql20 :refer [hfql]]))
  (:require
    [hyperfiddle.hfql19 :refer [hf-nav]]
    [hyperfiddle.incremental :as I :refer
     [sequenceI sequence-mapI bindI pureI fmapI capI joinI extend-seq]]
    #?(:clj [fixplz.expr :refer [unquote-via]])
    [meander.epsilon :as m]
    [minitest #?@(:clj [:refer [tests]])]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]
    #?(:clj [lexikon.core :refer [lexical-eval]])))

(tests
  (def >needle (pureI ""))
  (capI (fmapI submissions >needle)) := [9 10 11]
  (capI (sequenceI [(pureI 9) (pureI 10) (pureI 11)])) := [9 10 11]
  (capI (bindI (fmapI submissions >needle) pureI)) := [9 10 11])

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
  (cardinality '(shirt-sizes _)) := :db.cardinality/many
  (cardinality 'shirt-sizes) := :db.cardinality/many)

(defn many? [form] (= :db.cardinality/many (cardinality form)))

(tests
  (many? :dustingetz/gender) := false
  (many? 'shirt-sizes) := true
  (many? 'shirt-size) := false
  (many? '(shirt-sizes a)) := true
  (many? '(shirt-size a)) := false)

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol (name edge)) #_edge '%))

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form)
    `(~'fmapI (~'partial hf-nav ~?form) ~'%)

    (seq? ?form)
    (let [[f & args] ?form]
      `(~'fmapI ~f ~@args))))

#?(:clj
   (defn compile-hfql*
     "compile HFQL form to s-expressions in Incremental"
     [form]
     (m/match form

       [!pats ...]
       (apply merge (mapv compile-hfql* !pats))

       {& (m/seqable [?edge ?cont])}
       (let [edge* (compile-leaf* ?edge)]
         (if (many? ?edge)                                  ; thus % is sequential, (pureI [1 2 3])
           `{'~?edge
             (~'fmapI (~'fn [~'>as]
                        (~'vec                              ; emit associative structures, for path-centric manipulation
                          (~'for [~'% ~'>as]
                            (~'let [~(hf-edge->sym ?edge) ~'%]
                              ~(compile-hfql* ?cont)))))
               (~'extend-seq ~edge*))}
           `{'~?edge
             (~'let [~'% ~edge*]
               (~'let [~(hf-edge->sym ?edge) ~'%]
                 ~(compile-hfql* ?cont)))}))

       ?form
       `{'~?form ~(compile-leaf* ?form)})))

#?(:clj
   (defmacro hfql [form]
     (compile-hfql* form)))

(comment
  "cardinality strata"

  ; Find a plan for the dag such that all reachable cardinality/one edges are evaluated next, right here
  ; in applicative order ?
  ; is this fusion?

  (def submission:needle (inputI))
  (hfql [{(submissions a) :as submission
                          [{:dustingetz/email [:db/id
                                               {[(identity %) :as qqq]
                                                :db/ident}]}
                           {:dustingetz/gender
                            [{(shirt-sizes gender submission:needle)
                              [:db/id :db/ident]}]}
                           ]}])

  '(put hello-world/submissions:submission:needle "med")                             ; on the client


  (macroexpand-1 '(hfql [{(submissions a) :as submission
                          [{:dustingetz/email [:db/id
                                               {[(identity %) :as qqq]
                                                :db/ident}]}
                           {:dustingetz/gender
                            [{(shirt-sizes gender submission:email:qqq:ident:needle)
                              [:db/id :db/ident]}]}
                           ]}]))

  (let [a :dustingetz/male]
    (hfql [{(shirt-sizes a) [:db/ident]}]))
  := '{(shirt-sizes a) _}

  (def r (let [a (pureI "")]
           (hfql [{(submissions a)
                   [{:dustingetz/gender
                     [{(shirt-sizes gender email)
                       [:db/id :db/ident]}]}
                    :dustingetz/email]}])))
  := '{(submissions a) _}

  (as-> r %
    (I/sequence-at % [['(submissions a)]])
    (bindI % #(I/sequence-at % [['(submissions a) 0 :dustingetz/email]
                                #_['(submissions a) 0 :dustingetz/gender '(shirt-sizes gender)]
                                ['(submissions a) 1 :dustingetz/email]
                                #_['(submissions a) 1 :dustingetz/gender '(shirt-sizes gender)]]))
    #_(bindI % #(I/sequence-at % [['(submissions a) 0 :dustingetz/gender '(shirt-sizes gender) 0 :db/id]
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

(tests
  (macroexpand-1 '(hfql (identity a)))
  := '{(quote (identity a)) (fmapI identity a)}
  (let [a (pureI 1)] (hfql (identity a))) := '{(identity a) _}
  (-> *1 (get '(identity a)) capI) := 1

  (macroexpand-1 '(hfql {(identity >needle) (inc %)}))
  := '{(quote (identity >needle))
       (let [% (fmapI identity >needle)]
         (let [% %]
           {(quote (inc %)) (fmapI inc %)}))}
  (let [a (pureI 1)]
    (hfql {(identity a) (inc %)}))
  := '{(identity a) {(inc %) _}}

  (let [a (pureI 1)]
    (hfql
      {(identity a)
       [(dec %)
        (inc %)]}))
  := '{(identity a) {(dec %) _,
                     (inc %) _}}
  ; R2D2 traverse structure, click on links
  ;(capI *1) := 1

  (let [a (pureI 1)]
    (hfql {(inc a) (inc %)}))
  := '{(inc a) {(inc %) _}}
  (-> *1 (get '(inc a)) (get '(inc %)) capI) := 3

  (hf-nav :dustingetz/gender bob) := :dustingetz/male
  (macroexpand-1 '(hfql :dustingetz/gender))
  := '#:dustingetz{:gender (fmapI (partial hf-nav :dustingetz/gender) %)}
  (let [% (pureI bob)] (hfql :dustingetz/gender))
  (capI (:dustingetz/gender *1)) := :dustingetz/male

  (shirt-size :dustingetz/male) := 3
  (macroexpand-1 '(hfql (shirt-size %)))
  := '{(quote (shirt-size %)) (fmapI shirt-size %)}
  (let [% (pureI :dustingetz/male)] (hfql (shirt-size %))) := '{(shirt-size %) _}
  (-> *1 (get '(shirt-size %)) capI) := 3

  (macroexpand-1 '(hfql [:dustingetz/gender :db/id]))
  := '{(quote :dustingetz/gender) (fmapI (partial hf-nav :dustingetz/gender) %),
       (quote :db/id)             (fmapI (partial hf-nav :db/id) %)}
  (let [% (pureI bob)] (hfql [:dustingetz/gender :db/id])) := '{:dustingetz/gender _, :db/id _}
  (capI (:dustingetz/gender *1)) := :dustingetz/male

  (macroexpand-1 '(hfql {:dustingetz/gender [:db/id :db/ident]}))
  := '{(quote :dustingetz/gender)
       (let [% (fmapI (partial hf-nav :dustingetz/gender) %)]
         (let [gender %]
           {(quote :db/id)    (fmapI (partial hf-nav :db/id) %),
            (quote :db/ident) (fmapI (partial hf-nav :db/ident) %)}))}

  (let [% (pureI bob)] (hfql {:dustingetz/gender [:db/id :db/ident]}))
  := '#:dustingetz{:gender #:db{:id _, :ident _}}
  (capI (sequenceI (vals (select-keys (:dustingetz/gender *1) [:db/ident :db/id])))) := [:dustingetz/male 1]

  (macroexpand-1 '(hfql [{:dustingetz/gender [:db/id :db/ident]}]))
  := '{(quote :dustingetz/gender)
       (let [% (fmapI (partial hf-nav :dustingetz/gender) %)]
         (let [gender %]
           {(quote :db/id)    (fmapI (partial hf-nav :db/id) %),
            (quote :db/ident) (fmapI (partial hf-nav :db/ident) %)}))}

  (let [% (pureI bob)] (hfql [{:dustingetz/gender [:db/id :db/ident]}]))
  := '#:dustingetz{:gender #:db{:id _, :ident _}}
  (capI (sequenceI (vals (select-keys (:dustingetz/gender *1) [:db/ident :db/id])))) := [:dustingetz/male 1]

  (macroexpand-1 '(hfql [{:dustingetz/gender [{(shirt-size gender) [:db/id :db/ident]}]}]))
  := '(let [% (fmapI (partial hf-nav :dustingetz/gender) %)
            gender %]
        {(quote :dustingetz/gender)
         (let [% (fmapI shirt-size gender) % %]
           {(quote (shirt-size gender))
            #:db{:id    (fmapI (partial hf-nav :db/id) %),
                 :ident (fmapI (partial hf-nav :db/ident) %)}})})

  (let [% (pureI bob)] (hfql [{:dustingetz/gender [{(shirt-size gender) [:db/id :db/ident]}]}]))
  := '#:dustingetz{:gender {(shirt-size gender) #:db{:id _, :ident _}}}
  (capI (sequenceI (vals (select-keys (-> *1 :dustingetz/gender (get '(shirt-size gender))) [:db/ident :db/id]))))
  := [:dustingetz/mens-small 3]

  (macroexpand-1 '(hfql [{(submission needle)
                          [{:dustingetz/gender
                            [{(shirt-size gender)
                              [:db/id :db/ident]}]}]}]))
  := '{(quote (submission needle))
       (let [% (fmapI submission needle)]
         (let [% %]
           {(quote :dustingetz/gender)
            (let [% (fmapI (partial hf-nav :dustingetz/gender) %)]
              (let [gender %]
                {(quote (shirt-size gender))
                 (let [% (fmapI shirt-size gender)]
                   (let [% %]
                     {(quote :db/id)    (fmapI (partial hf-nav :db/id) %),
                      (quote :db/ident) (fmapI (partial hf-nav :db/ident) %)}))}))}))}

  (let [needle (pureI "alice")]
    (hfql [{(submission needle)
            [{:dustingetz/gender
              [{(shirt-size gender)
                [:db/id :db/ident]}]}]}]))
  := '{(submission needle) #:dustingetz{:gender {(shirt-size gender) #:db{:id _, :ident _}}}}
  (-> *1 (get '(submission needle)) :dustingetz/gender
    (get '(shirt-size gender)) :db/ident capI) := :dustingetz/womens-small

  (macroexpand-1 '(hfql {:dustingetz/gender [:db/id (:db/ident %)]}))
  := '{(quote :dustingetz/gender)
       (let [% (fmapI (partial hf-nav :dustingetz/gender) %)]
         (let [gender %]
           {(quote (:db/ident %)) (fmapI :db/ident %),
            (quote :db/id)        (fmapI (partial hf-nav :db/id) %)}))}

  (macroexpand-1 '(hfql {(:dustingetz/gender %) [(:db/id %) (:db/ident %)]}))
  := '{(quote (:dustingetz/gender %))
       (let [% (fmapI :dustingetz/gender %)]
         (let [% %]
           {(quote (:db/id %))    (fmapI :db/id %),
            (quote (:db/ident %)) (fmapI :db/ident %)}))}
  )
