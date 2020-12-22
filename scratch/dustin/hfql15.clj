(ns dustin.hfql15
  (:refer-clojure :exclude [sequence])
  (:require [dustin.fiddle :refer :all]
            [dustin.hf-nav :refer :all]
            [dustin.monad-scope :as ms :refer [bind pure runScope sequence]]
            [hyperfiddle.fabric :as df]
            [meander.epsilon :as m :refer [match]]
            [minitest :refer [tests]]
            [promesa.core :as p]))


; State, commit to something
; Reactions
; Bind reactions

(defmacro trace
  ([sym]
   `(trace ~sym ~sym))
  ([tag sym]
   (let [{:keys [:line :column]} (meta &form)]
     `(fn [& args#]
        (prn '~tag ~(str "@" line ":" column) " := " args#)
        (apply ~sym args#)))))

(defmacro safe [f]
  `(fn [& args#]
     (try
       (apply ~f args#)
       (catch Exception err#
         (prn err#)
         (throw err#)))))

(defn fmap [f & as]
  (cond
    (every? #(instance? hyperfiddle.View %) as) (apply df/fmap f as)
    (every? fn? as)                             (apply ms/fmap f as)
    :else                                       #_(assert false) (apply f as)))

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn hf-nav* [edge a]
  ((safe hf-nav) edge a))

(defn hf-apply [edge a scope]
  (cond
    (keyword? edge) (hf-nav* edge a)
    (= '* edge)     (touch1 a)
    (seq? edge)     (let [[f & args] edge]
                      (apply (safe (clojure.core/resolve f)) (replace scope args)))
    ()              (println "hf-eval unmatched edge: " edge)))

(defn hf-eval [edge >a]
  (fn [scope]
    (let [>b (fmap (fn [a] (hf-apply edge a scope)) >a)]
      [{(hf-edge->sym edge) >b, '% >b}
       >b])))

(declare hf-pull' hf-pull-many')

; Lose the cardinality
; Use promises

(defn hf-pull' [pat SRa]                                    ; unwind this R, not needed, only for needle
  (match pat
    [!pats ...]
    (->> !pats
      (mapv (fn [pat] (hf-pull' pat SRa)))
      (ms/sequence)
      (ms/fmap (fn [Ras] (apply df/fmap merge Ras))))       ; p/map


    ; easy with promises cardinality one
    {& (m/seqable [?edge ?pat])}
    (as-> SRa SRa
      (bind SRa (partial hf-eval ?edge))

      (bind SRa                                             ; Scope Reactive a
        (fn [Ra]                                            ; Reactive a
          (let [RSRb                                        ; Reactive Scope Reactive b
                (->> Ra (df/fmap #_ p/map
                          (fn [a]
                            ; Why is this happening?
                            ; Because the scope is producing streams at each layer
                            (let [SRb (hf-pull-many' ?pat a)]
                              SRb))))]                      ; put Scope b into the stream
            @RSRb)))
      #_(fmap (fn [>a] (fmap (fn [a] {?edge a}) >a)) SRa))


    ?leaf
    (as-> SRa ma
      (bind ma (partial hf-eval ?leaf))
      (fmap (fn [>a] (fmap (fn [a] {?leaf a}) >a)) ma))))

(defn hf-pull-many' [?pat a]
  (if (vector? a)
    (ms/sequence (mapv #(hf-pull' ?pat (ms/pure %)) a))
    (hf-pull' ?pat (ms/pure a))))

(defn hf-pull [pat >a & [scope]]
  (let [[_ >b] (runScope (hf-pull' pat (ms/pure >a)) (or scope {}))]
    >b))

; feed it a scope, get a Reaction
; scope contains streams so they can update individually

(defn do-promise [>x v]
  (let [p (p/create
            (fn [resolve! reject!]
              (df/on >x resolve!)
              nil))]
    (df/put >x v)
    p))

(comment

  (def >v (df/input))
  (def >y (hf-pull :dustingetz/gender >v {}))
  (def py (do-promise >y))
  (df/put >x 42)
  (inc 1)
  @py
  := {'(identity %) 42}
  )

(comment

  (hf-pull :dustingetz/gender (p/resolved 17592186045429) {})
  := (p/resolved {:dustingetz/gender :dustingetz/male})
  )

(comment

  (with-input 17592186045429
    (hf-pull :dustingetz/gender input))
  := '#:dustingetz{:gender :dustingetz/male}

  (with-input 17592186045429
    (hf-pull [{:dustingetz/gender [:db/ident]}] input))
  := '#:dustingetz{:gender #:db{:ident :dustingetz/male}}


  (with-input dustin.dev/*$*
    (hf-pull (gender $) nil {'$ input}))
  := '{(gender $) 17592186045418}

  (with-input 17592186045428
    (hf-pull :dustingetz/tags input {}))
  := #:dustingetz{:tags #{:c :b :a}}

  (with-input "alice"
    (hf-pull {(submission >needle) [:dustingetz/tags]} nil {'>needle input}))

  := {(submission >needle) #:dustingetz{:tags #{:c :b :a}}}

  (with-input "example"
    (hf-pull {(submissionS >needle) [:dustingetz/tags]} nil {'>needle input}))
  := {(submission >needle) #:dustingetz{:tags #{:c :b :a}}}

  (with-input "example"
    (hf-pull (submissionS >needle) nil {'>needle input}))
  := '{(submissionS needle) [17592186045428 17592186045429 17592186045430]}

  ;; TAG
  (with-input "example"
    (hf-pull {(submissionS >needle) [:dustingetz/gender]} nil {'>needle input}))
  := '{(submissionS needle) [#:dustingetz{:gender :dustingetz/female}
                             #:dustingetz{:gender :dustingetz/male}
                             #:dustingetz{:gender :dustingetz/male}]}

  (with-input "example"
    (hf-pull {(submissionS >needle) [{:dustingetz/gender [:db/ident]}]} nil {'>needle input}))
  := '{(submissionS needle) [#:dustingetz{:gender #:db{:ident :dustingetz/female}}
                             #:dustingetz{:gender #:db{:ident :dustingetz/male}}
                             #:dustingetz{:gender #:db{:ident :dustingetz/male}}]}

  (with-input "example"
    (hf-pull {(submissionS needle) {:dustingetz/gender [(shirt-size dustingetz/gender)]}} nil {'needle input}))
  := '{(submissionS needle) [#:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045425}}
                             #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045421}}
                             #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045421}}]}

  (with-input "example"
    (hf-pull {(submissionS needle) [{:dustingetz/gender [{(shirt-size dustingetz/gender) [:db/id]}]}]} nil {'needle input}))
  := '{(submissionS needle) [#:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:id 17592186045425}}}
                             #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:id 17592186045421}}}
                             #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:id 17592186045421}}}]}

  (with-input "example"
    (hf-pull
      {(submissionS needle)
       [{:dustingetz/gender
         [{(shirt-size dustingetz/gender)
           [:db/ident]}]}]}
      nil
      {'needle input}))

  := '{(submissionS needle) [#:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/womens-medium}}}
                             #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/mens-small}}}
                             #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/mens-small}}}]}

  (with-input 17592186045418
    (hf-pull [{:db/ident [:db/id]}] input))
  := #:db{:ident #:db{:id 17592186045418}}

  (with-input 17592186045421
    (hf-pull [{:dustingetz/gender [{:db/ident [:db/id]}]}] input))
  := #:dustingetz{:gender #:db{:ident #:db{:id 17592186045418}}}

  (with-input 17592186045421
    (hf-pull [{:dustingetz/gender [{:db/ident [{:db/ident [:db/ident]}]}]}] input))
  := #:dustingetz{:gender #:db{:ident #:db{:ident #:db{:ident :dustingetz/male}}}}

  (with-input 17592186045421
    (hf-pull [{:dustingetz/gender [:db/id]}] input))
  := #:dustingetz{:gender #:db{:id 17592186045418}}

  (with-input 17592186045421
    (hf-pull [{:dustingetz/gender [{:db/id [:db/id]}]}] input))
  := #:dustingetz{:gender #:db{:id #:db{:id 17592186045418}}}


  (with-input 17592186045421
    (hf-pull [{:dustingetz/gender [{:db/id [{:db/id [{:db/id [:db/id]}]}]}]}] input))
  := #:dustingetz{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045418}}}}}

  )
