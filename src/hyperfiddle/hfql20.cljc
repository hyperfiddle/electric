(ns hyperfiddle.hfql20
  #?(:cljs (:require-macros [hyperfiddle.hfql20 :refer [hfql]]
                            [minitest :refer [tests]]))
  (:require
    [datascript.core :as d]                                 ;#?(:clj [datomic.api :as d])
    [hyperfiddle.incremental :refer
     [sequenceI sequence-mapI bindI pureI fmapI capI joinI extend-seq]]
    [meander.epsilon :as m]
    [minitest :refer [tests]]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]
    [hyperfiddle.incremental :as I]))

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

(defn hf-nav [kf ref]
  ; emits smart refs
  (kf (d/entity hyperfiddle.api/*$* ref)))

(tests
  (hf-nav :db/ident 3) := :dustingetz/mens-small
  (hf-nav :db/id 3) := 3
  ;(hf-nav identity [:dustingetz/email "alice@example.com"]) := #:db{:id 9}
  (hf-nav :db/id [:dustingetz/email "alice@example.com"]) := 9
  (hf-nav :dustingetz/gender [:dustingetz/email "alice@example.com"]) := :dustingetz/female)

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
         (if (many? ?edge)                                     ; thus % is sequential, (pureI [1 2 3])
           `{'~?edge
             (~'fmapI (~'fn [~'>as]
                        (~'vec                              ; emit associative structures, for path-centric manipulation
                          (~'for [~'% ~'>as]
                            (~'let [~(hf-edge->sym ?edge) ~'%]
                              ~(compile-hfql* ?cont)))))
               (~'extend-seq ~edge*))}                        ; extend monad
           `{'~?edge
             (~'let [~'% ~edge*]
               (~'let [~(hf-edge->sym ?edge) ~'%]
                 ~(compile-hfql* ?cont)))}))

       ?form
       `{'~?form ~(compile-leaf* ?form)})))

#?(:clj
   (defmacro hfql [form]
     (compile-hfql* form)))

(tests
  (let [a :dustingetz/male]
    (hfql [{(shirt-sizes a) [:db/ident]}]))
  := '{(shirt-sizes a) _}
  )

#?(:clj
   (tests
     "cardinality many"
     (shirt-sizes :dustingetz/male) := [3 4 5]
     (many? '(shirt-sizes a)) := true

     ;(compile-hfql* '[{(shirt-sizes a) [:db/ident]}])
     (macroexpand-1 '(hfql [{(shirt-sizes a) [:db/ident]}]))
     (let [a (pureI :dustingetz/male)]
                 (hfql [{(shirt-sizes a) [:db/ident]}]))
     (-> *1 (get '(shirt-sizes a)))
     (capI (joinI (fmapI sequenceI (fmapI #(map :db/ident %) *1))))
     := [:dustingetz/mens-small
         :dustingetz/mens-medium
         :dustingetz/mens-large]
     ; the goal of above is to produce the identity structure, no streams
     ; without removing levels of the map

     ;(capI (unsequenceI (pureI [1 2 3])))
     ;(capI (sequenceI *1))

     ;(let [a (pureI "")]
     ;  (compile-hfql [{(submissions a)
     ;                  [{:dustingetz/gender
     ;                    [{(shirt-sizes gender)
     ;                      [:db/id :db/ident]}]}]}]))
     ;(lexical-eval {'a (pureI "")} *1)
     ;(def x *1)
     ;(-> x
     ;  (get '(submissions a))
     ;  (->> (fmapI (fn [a]
     ;                (println a)
     ;                (sequenceI
     ;                  (map
     ;                    (comp
     ;                      sequence-mapI
     ;                      #(get % '(shirt-sizes gender))
     ;                      :dustingetz/gender)
     ;                    a))))
     ;    joinI)
     ;  capI
     ;  #_sequenceMapI
     ;  #_capI)
     ))

(tests
  (macroexpand-1 '(hfql (identity a)))
  := '{(quote (identity a)) (fmapI identity a)}
  (let [a (pureI 1)] (hfql (identity a)))
  := '{(identity a) _}
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
  := '{(identity a) {(dec %) _, (inc %) _}}
  ; R2D2 traverse structure, click on links

  (let [a (pureI 1)]
    (hfql {(inc a) (inc %)}))
  := '{(inc a) {(inc %) _}}
  (-> *1 (get '(inc a)) (get '(inc %)) capI) := 3

  (hf-nav :dustingetz/gender bob) := :dustingetz/male
  (macroexpand-1 '(hfql :dustingetz/gender))
  := '{(quote :dustingetz/gender) (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/gender) %)}
  (let [% (pureI bob)] (hfql :dustingetz/gender))
  (capI (:dustingetz/gender *1)) := :dustingetz/male

  (shirt-size :dustingetz/male) := 3
  (macroexpand-1 '(hfql (shirt-size %)))
  := '{(quote (shirt-size %)) (fmapI shirt-size %)}
  (let [% (pureI :dustingetz/male)] (hfql (shirt-size %)))
  := '{(shirt-size %) _}
  (-> *1 (get '(shirt-size %)) capI) := 3

  (macroexpand-1 '(hfql [:dustingetz/gender :db/id]))
  := '{(quote :dustingetz/gender) (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/gender) %),
       (quote :db/id)             (fmapI (partial hyperfiddle.hfql20/hf-nav :db/id) %)}
  (let [% (pureI bob)] (hfql [:dustingetz/gender :db/id]))
  := '{:dustingetz/gender _, :db/id _}
  (capI (:dustingetz/gender *1)) := :dustingetz/male

  (macroexpand-1 '(hfql {:dustingetz/gender [:db/id :db/ident]}))
  := '{(quote :dustingetz/gender)
       (let [% (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/gender) %)]
         (let [gender %]
           {(quote :db/id)    (fmapI (partial hyperfiddle.hfql20/hf-nav :db/id) %),
            (quote :db/ident) (fmapI (partial hyperfiddle.hfql20/hf-nav :db/ident) %)}))}

  (let [% (pureI bob)] (hfql {:dustingetz/gender [:db/id :db/ident]}))
  := '#:dustingetz{:gender #:db{:id _, :ident _}}
  (capI (sequenceI (vals (select-keys (:dustingetz/gender *1) [:db/ident :db/id]))))
  := [:dustingetz/male 1]

  (macroexpand-1 '(hfql [{:dustingetz/gender [:db/id :db/ident]}]))
  := '{(quote :dustingetz/gender)
       (let [% (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/gender) %)]
         (let [gender %]
           {(quote :db/id)    (fmapI (partial hyperfiddle.hfql20/hf-nav :db/id) %),
            (quote :db/ident) (fmapI (partial hyperfiddle.hfql20/hf-nav :db/ident) %)}))}

  (let [% (pureI bob)] (hfql [{:dustingetz/gender [:db/id :db/ident]}]))
  := '#:dustingetz{:gender #:db{:id _, :ident _}}
  (capI (sequenceI (vals (select-keys (:dustingetz/gender *1) [:db/ident :db/id])))) := [:dustingetz/male 1]

  (macroexpand-1 '(hfql [{:dustingetz/gender [{(shirt-size gender) [:db/id :db/ident]}]}]))
  := '{(quote :dustingetz/gender)
       (let [% (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/gender) %)]
         (let [gender %]
           {(quote (shirt-size gender))
            (let [% (fmapI shirt-size gender)]
              (let [% %]
                {(quote :db/id)    (fmapI (partial hyperfiddle.hfql20/hf-nav :db/id) %),
                 (quote :db/ident) (fmapI (partial hyperfiddle.hfql20/hf-nav :db/ident) %)}))}))}

  (let [% (pureI bob)] (hfql [{:dustingetz/gender [{(shirt-size gender) [:db/id :db/ident]}]}]))
  := '#:dustingetz{:gender {(shirt-size gender) #:db{:id _, :ident _}}}
  (capI (sequenceI (vals (select-keys (-> *1 :dustingetz/gender (get '(shirt-size gender))) [:db/ident :db/id]))))
  := [:dustingetz/mens-small 3]

  (macroexpand-1
    '(hfql
       [{(submission needle)
         [{:dustingetz/gender
           [{(shirt-size gender)
             [:db/id :db/ident]}]}]}]))
  := '{(quote (submission needle))
       (let [% (fmapI submission needle)]
         (let [% %]
           {(quote :dustingetz/gender)
            (let [% (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/gender) %)]
              (let [gender %]
                {(quote (shirt-size gender))
                 (let [% (fmapI shirt-size gender)]
                   (let [% %]
                     {(quote :db/id) (fmapI (partial hyperfiddle.hfql20/hf-nav :db/id) %),
                      (quote :db/ident) (fmapI (partial hyperfiddle.hfql20/hf-nav :db/ident) %)}))}))}))}

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
       (let [% (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/gender) %)]
         (let [gender %]
           {(quote (:db/ident %)) (fmapI :db/ident %),
            (quote :db/id) (fmapI (partial hyperfiddle.hfql20/hf-nav :db/id) %)}))}

  (macroexpand-1 '(hfql {(:dustingetz/gender %) [(:db/id %) (:db/ident %)]}))
  := '{(quote (:dustingetz/gender %))
       (let [% (fmapI :dustingetz/gender %)]
         (let [% %]
           {(quote (:db/id %)) (fmapI :db/id %),
            (quote (:db/ident %)) (fmapI :db/ident %)}))}
  )
