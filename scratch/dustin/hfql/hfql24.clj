(ns dustin.hfql.hfql24
  (:require
    [minitest :refer [tests]]
    [datascript.core :as d]                                 ;#?(:clj [datomic.api :as d])
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]
    [meander.epsilon :as meander]
    [missionary.core :as m]
    [hfdl.lib :refer [reactive-for]]
    [hfdl.lang :refer [dataflow debug! result]]
    [clojure.string :as str]))


(def cardinality* {`submissions :db.cardinality/many
                   `shirt-sizes :db.cardinality/many
                   `genders     :db.cardinality/many})

(defn cardinality [form]
  (cond
    (symbol? form) (get cardinality* form)
    (keyword? form) (get cardinality* form)
    (seq? form) (let [[f & args] form]
                  (get cardinality* f)
                  #_(:db/cardinality (meta f)))))

(tests
  (cardinality `(shirt-sizes _)) := :db.cardinality/many
  (cardinality `shirt-sizes) := :db.cardinality/many)

(defn many? [form] (= :db.cardinality/many (cardinality form)))

(tests
  (many? :dustingetz/gender) := false
  (many? `shirt-sizes) := true
  (many? `shirt-size) := false
  (many? `(shirt-sizes a)) := true
  (many? `(shirt-size a)) := false)

; entity navigation

(defn hf-nav [kf ref]
  ; emits smart refs
  (kf (d/entity hyperfiddle.api/*$* ref)))

(tests
  (:db/ident (d/touch (datascript.core/entity hyperfiddle.api/*$* 3)))
  := :dustingetz/mens-small

  (hf-nav :db/ident 3) := :dustingetz/mens-small
  (hf-nav :db/id 3) := 3
  ;(hf-nav identity [:dustingetz/email "alice@example.com"]) := #:db{:id 9}
  (hf-nav :db/id [:dustingetz/email "alice@example.com"]) := 9
  (hf-nav :dustingetz/gender [:dustingetz/email "alice@example.com"]) := :dustingetz/female)

; HFQL

(defn hf-edge->sym [edge]
  (if (keyword? edge)
    (if (namespace edge)
      (symbol (str (namespace edge) "__" (name edge)))
      (symbol (name edge)))
    '%))

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form) `(hf-nav ~?form ~'%)
    (seq? ?form)     ?form))

(defn compile-hfql*
  "compile HFQL form to s-expressions in Incremental"
  [form]
  (meander/match form

    [!pats ...]
    (apply merge (mapv compile-hfql* !pats))

    {& (meander/seqable [?edge ?cont])}
    (let [edge* (compile-leaf* ?edge)]
      (if (many? ?edge)                                     ; thus % is sequential, (pureI [1 2 3])
        `{'~?edge
          @(reactive-for (~'fn [~'%]
                            (dataflow
                             (~'let [~(hf-edge->sym ?edge) ~'%]
                              ~(compile-hfql* ?cont))))
            (~'unquote ~edge*))}
        `{'~?edge
          (~'let [~'% ~edge*]
           (~'let [~(hf-edge->sym ?edge) ~'%]
            ~(compile-hfql* ?cont)))}))

    ?form
    `{'~?form ~(compile-leaf* ?form)}))

(defmacro hfql [form]
  (compile-hfql* form))

(tests
 (macroexpand-1 '(hfql {(dustin.fiddle/genders) [:db/ident]}))
 := '{'(dustin.fiddle/genders)
      @(hfdl.lib/reactive-for
        (fn [%] (hfdl.lang/dataflow
                (let [% %] {':db/ident (dustin.hfql.hfql24/hf-nav :db/ident %)})))
        (unquote (dustin.fiddle/genders)))})

(tests
 (def program (dataflow (hfql {(dustin.fiddle/genders) [:db/ident]})))

 (def process (debug! program))
 (result program @process) := `{(genders) [{:db/ident :dustingetz/male}
                                           {:db/ident :dustingetz/female}]})

(tests
 (def !gender (atom :dustingetz/male))
 (def >gender (m/watch !gender))
 (def program (dataflow (let [gender @>gender]
                          (hfql {(dustin.fiddle/shirt-size gender) [:db/id :db/ident]}))))
 (def process (debug! program))

 (result program @process) := '{(dustin.fiddle/shirt-size gender) {:db/id 3 :db/ident :dustingetz/mens-small}}

 )

(tests

 (def !needle (atom ""))
 (def >needle (m/watch !needle))
 (def program (dataflow
               (let [needle @>needle]
                 (hfql [{(dustin.fiddle/submissions needle)
                         [:dustingetz/email
                          {:dustingetz/gender
                           [:db/ident
                            {(dustin.fiddle/shirt-sizes dustingetz__gender) [:db/ident]}]}]}
                        {(dustin.fiddle/genders) [:db/ident]}]))))

 (def process (debug! program))

 (result program @process)
 :=
 '{(dustin.fiddle/submissions needle)
   [#:dustingetz{:email "alice@example.com"
                 :gender
                 {(dustin.fiddle/shirt-sizes dustingetz__gender)
                  [#:db{:ident :dustingetz/womens-small}
                   #:db{:ident :dustingetz/womens-medium}
                   #:db{:ident :dustingetz/womens-large}]
                  :db/ident :dustingetz/female
                  }}
    #:dustingetz{:email "bob@example.com"
                 :gender
                 {(dustin.fiddle/shirt-sizes dustingetz__gender)
                  [#:db{:ident :dustingetz/mens-small}
                   #:db{:ident :dustingetz/mens-medium}
                   #:db{:ident :dustingetz/mens-large}]
                  :db/ident :dustingetz/male}}
    #:dustingetz{:email "charlie@example.com"
                 :gender
                 {(dustin.fiddle/shirt-sizes dustingetz__gender)
                  [#:db{:ident :dustingetz/mens-small}
                   #:db{:ident :dustingetz/mens-medium}
                   #:db{:ident :dustingetz/mens-large}]
                  :db/ident :dustingetz/male }}]
   (dustin.fiddle/genders) [#:db{:ident :dustingetz/male}
                            #:db{:ident :dustingetz/female}]}

 (reset! !needle "alice")

 (result program @process)
 :=
 '{(dustin.fiddle/submissions needle)
   [#:dustingetz{:email "alice@example.com"
                 :gender
                 {:db/ident :dustingetz/female
                  (dustin.fiddle/shirt-sizes dustingetz__gender)
                  [#:db{:ident :dustingetz/womens-small}
                   #:db{:ident :dustingetz/womens-medium}
                   #:db{:ident :dustingetz/womens-large}]}}]
   (dustin.fiddle/genders) [#:db{:ident :dustingetz/male}
                            #:db{:ident :dustingetz/female}]}

 )


;; Some issues:
;; - clojure.core/let don't accept namespaced symbols (I'm cheating using ns__name instead of ns/name).
;; - no auto-resolving of ns-mapped and ns-aliased symbols, it might just be a missing macroexpansion trick,
;; - inputs are not well defined, @>needle is incorrect, it should be >needle and be late-bound.
