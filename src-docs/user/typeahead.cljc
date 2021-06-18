(ns user.typeahead
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


(defnode option [])
(defnode typeahead-select [])

(defnode example-form [db e]
  (form
    (field
      ::hf/label "shirt size"
      (typeahead-select
        ::hf/eav [e :dustingetz/shirt-size]
        ::options (node [needle]
                    (let [gender (thread (:dustingetz/gender (datomic.api/entity db e)))
                          es (thread
                               (datomic.api/q
                                 '[:in $ ?gender :find [?e ...] :where
                                   [?e :dustingetz/type :dustingetz/shirt-size]
                                   [?e :dustingetz/gender ?gender]]
                                 db gender (or needle "")))]
                      (for [e es]
                        (option ~@(thread (:db/ident (datomic.api/entity db e)))))))))))

; everything is async in dataflow context, blocking is forbidden.

(with-redefs [datomic.api/entity ...]
  (example-form ...))
