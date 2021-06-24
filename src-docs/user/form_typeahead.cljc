(ns user.form-typeahead
  (:require [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode vars system]]
            [missionary.core :as m]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql exports]]
            [hfdl.impl.util :as u]))

(defn dom-listen [!e]
  #?(:cljs (fn [!]
             (goog.events/addEventListener !e !)
             #(goog.events/removeEventListener !e !))))

(defnode form []
  (let [!el (js/document.createElement "form")]
    (.appendChild *!parent !el)
    ~(m/observe (dom-listen !el))))

(defnode div []
  (let [!el (.createElement js/document "div")]
    (.appendChild *!parent !el)
    ~(m/observe (dom-listen !el))))

(defnode label [v]
  (let [!el (.createElement js/document "label")]
    (.appendChild *!parent !el)
    ~(m/observe (dom-listen !el))))

(defnode field []
  (div (label _)
       body)

  (let [!el (.createElement js/document "div")]
    (.appendChild *!parent !el)
    ~(m/observe (dom-listen !el))))

(defmacro thread [& body]
  `(m/ap (m/? (m/via m/blk ~@body))))

(defn entity-gender [db e]
  (thread (:dustingetz/gender (datomic.api/entity db e))))

(defn entity-ident [db e]
  (thread (:db/ident (datomic.api/entity db e))))

(defn shirt-sizes [db gender needle]
  (thread
    (datomic.api/q
      '[:in $ ?gender :find [?e ...] :where
        [?e :dustingetz/type :dustingetz/shirt-size]
        [?e :dustingetz/gender ?gender]]
      db gender (or needle ""))))

(defnode option [])
(defnode typeahead-select [])

(defnode example-form [db e]
  (form
    (field
      ::hf/label "shirt size"
      (typeahead-select
        ::hf/eav [e :dustingetz/shirt-size]
        ::options (node [needle]
                    (let [gender ~(entity-gender db e)]
                      (for [e ~(shirt-sizes db gender needle)]
                        (option ~@~(entity-ident db e)))))))))

; everything is async in dataflow context, blocking is forbidden.

(with-redefs [datomic.api/entity ...]
  (example-form ...))
