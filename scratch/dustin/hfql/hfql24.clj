(ns dustin.hfql.hfql24
  (:require
    [minitest :refer [tests]]
    [datascript.core :as d]                                 ;#?(:clj [datomic.api :as d])
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]
    #_[meander.epsilon :as ...]
    [missionary.core :as m]
    ))


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

(tests

  (shirt-sizes :dustingetz/male "")
  ;(shirt-size :dustingetz/male)

  (declare hfql)


  (def >z
    (let [>gender (m/ap :dustingetz/male)
          >needle ...]
      (hfql [{(shirt-sizes >gender >needle)
              [:db/id :db/ident]
              (genders) [:db/ident]}])))

  (def !z (>z #() #()))
  @!z := {'(shirt-sizes >gender >needle)
          [{:db/id 3 :db/ident :dustingetz/mens-small}
           {:db/id 4 :db/ident :dustingetz/mens-medium}
           {:db/id 5 :db/ident :dustingetz/mens-large}]}

  )



#_(defn hf-edge->sym [edge]
    (if (keyword? edge) (symbol (name edge)) #_edge '%))