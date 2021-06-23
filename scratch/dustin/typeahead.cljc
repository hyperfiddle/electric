(ns user.crud3
  (:require [datascript.core :as d]
            [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode node vars main]]
            [missionary.core :as m]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql]]))

(defnode form [& args])
(defnode submit [& args])
(defnode field [& args])
(defnode text [& args])

(defnode select-widget [& {:keys [::db ::eav ::options ::render-option]}]
  (let [!needle (atom "")]                                  ; widget local state
    [:<> [:input.select-needle {:value ~(m/watch !needle) :on-change #(reset! !needle %)}]
     [:select {:selected ~(m/watch ref) :on-change #(reset! ref %)}
      ~@(for [x ~(options @!needle)]
          (render-option x))]]))

(defn dom-listen [!e]
  #?(:cljs (fn [!]
             (goog.events/addEventListener !e !)
             #(goog.events/removeEventListener !e !))))

(declare rfor *!parent)

(defnode option [v & {:keys []}]
  (let [!e (js/document.createElement "option" v)]
    (.appendChild *!parent !e)
    ~(m/observe (dom-listen !e))))

(defnode text [v]
  (let [!e (doto (.createElement js/document "input")
             (.setAttribute "type" "text")
             (.setAttribute !e "value" v))]
    (.appendChild *!parent !e)
    ~(m/observe (dom-listen !e))))

(defnode select [& {:keys [::options]}]
  (let [!needle (atom "")
        needle ~(m/watch !needle)
        !e (doto (.createElement js/document "input")
             (.setAttribute "type" "select")
             (.setAttribute !e "value" v))]
    (binding [*!parent !e]
      ~@(options needle))
    (.appendChild *!parent !e)
    ~(m/observe (dom-listen !e))))

;(defnode typeahead-select [& {:keys [::eav ::options]}]
;  (let [!needle (atom "")
;        [e v] eav
;        !e (doto (.createElement js/document "input")
;             (.setAttribute "type" "select")
;             (.setAttribute !e "value" v))]
;    (select ::eav eav)
;    (binding [*!parent !e]
;      ~@(rfor [x ~(options !needle)]
;              (render-option x)))
;    (.appendChild *!parent !e)
;    ~(m/observe (dom-listen !e))))

;(defnode typeahead-select [& {:keys [::db ::eav ::options ::render-option]}]
;  (let [[e v] eav
;        !e (doto (.createElement js/document "input")
;             (.setAttribute "type" "select")
;             (.setAttribute !e "value" v))]
;    (binding [*!parent !e]
;      ~@(rfor [x ~(options !needle)]
;          (render-option x)))
;    (.appendChild *!parent !e)
;    ~(m/observe (dom-listen !e))))

; Query the view, wait for user interactions, emit datomic transaction values.

(defnode email-taken [db email]
  (some? (d/q '[:in ?x $ :find ?e . :where [?e :dustingetz/email ?x]] db email)))

(defnode example-form [db transact! e]
  (form
    (field
      ::hf/label "email"
      ::hf/is-invalid (let [x (:dustingetz/email !e)] ~@(email-taken db x))
      (text ::hf/eav [e :dustingetz/email]))

    (field
      ::hf/label "gender"
      (typeahead-select
        ::hf/db db
        ::hf/eav [e :dustingetz/gender]
        ::hf/options (node [_] [:dustingetz/male :dustingetz/female])
        ::hf/render-option (node [x] (name x))))

    (field
      ::hf/label "shirt size"
      (typeahead-select
        ::hf/eav [e :dustingetz/shirt-size]
        ::options (node [needle]
                    (d/q '[:in $ ?gender :find [?e ...] :where
                           [?e :dustingetz/type :dustingetz/shirt-size]
                           [?e :dustingetz/gender ?gender]]
                         db (:dustingetz/gender !e) (or needle "")))
        ::render-option (node [option]
                          (option ~@(:db/ident (d/entity db option))))))

    (submit ::on-click #(transact!) "save!")))

(tests
  (def *conn*)
  (def !db (atom []))
  (def !e (atom [:dustingetz/email "alice@example.com"]))
  (def sampler (test-dag (let [db ~(m/watch !db)
                               transact! (partial d/transact db)
                               datoms (example-form db ~(m/watch e) transact!)]
                           (reset! !db (datomic.api/with db datoms)))))
  @sampler := 3)
