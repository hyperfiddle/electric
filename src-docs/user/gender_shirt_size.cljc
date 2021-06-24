(ns user.gender-shirt-size
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            [hfdl.lang :as p :refer [#?(:clj defnode) vars system]]
            [hfdl.impl.util :as u]
            [hyperfiddle.api :as hf :refer [*$*]]
            [hyperfiddle.q2 :refer [hf-nav hfql exports]]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs
     (:require-macros
       [user.gender-shirt-size
        :refer [ref2 render-form page-submissions]])))

(def q (comp #(m/ap (m/? (m/via m/blk %))) d/q))

(defnode genders []
  (into [] ~(q '[:find [?e ...] :where [_ :person/gender ?e]] *$*)))

;(tests
;  (genders) := [:person/male :person/female] #_[male female])

(defnode gender []
  (first (genders)))

;(tests
;  (gender) := :person/male #_male)

(defn shirt-sizes [gender needle]
  (sort
    (d/q
      '[:in $ % ?gender ?needle
        :find [?e ...]
        :where
        [?e :person/type :person/shirt-size]
        [?e :person/gender ?gender]
        [?e :db/ident ?ident]
        (hyperfiddle.api/needle-match ?ident ?needle)]
      *$* hf/rules gender (or needle ""))))

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?) :ret sequential?)

(defn submissions [& [needle]]
  ;(m/via m/cpu)
  (sort
    (d/q '[:find [?e ...]
           :in $ % ?needle
           :where
           [?e :person/email ?email]
           (hyperfiddle.api/needle-match ?email ?needle)]
         *$* hf/rules (or needle ""))))

(s/fdef submissions :args (s/cat :needle string?) :ret sequential?)

;(tests
;  (submissions) := [alice bob charlie]
;  (submissions "example") := [alice bob charlie]
;  (submissions "b") := [bob])

(defnode ref2 [v {::hf/keys [options]}]
  (dom/select v (p/for [x options] (dom/option x))))

(defnode render-form [xs]
  #_(let [!needle (atom "")]
    (dom/div
      (dom/h1 "submissions")
      (dom/form
        (dom/field
          (dom/span "needle")
          (dom/input !needle)))
      ))
  (dom/table
    (p/for [{:keys [:db/id
                    :person/email
                    :person/gender
                    :person/shirt-size]} xs #_(xs ~(m/watch !needle))]
      (dom/tr
        (dom/td (dom/span (pr-str id)))
        (dom/td email)
        (dom/td (dom/span (pr-str gender)))
        (dom/td (dom/span (pr-str shirt-size)))))))

#_
(defnode page-submission-detail "" [e]
  (hfql
    [{e
      [:db/id
       :person/email
       {(:person/gender ::hf/options (genders)
          ::hf/render ref) [:db/ident]}
       {(:person/shirt-size ::hf/options (shirt-sizes person/gender _)
          ::hf/render ref) [:db/ident]}]}]))

(defnode render-text [x opts]
  (dom/input x))

(defnode page-submissions "" []
  (hfql
    [{((submissions "") ::hf/render render-form)
      [:db/id
       (:person/email ::hf/render render-text)
       {(:person/gender ::hf/options (genders) ::hf/render ref2) [:db/ident]}
       {(:person/shirt-size ::hf/options (shirt-sizes :dustingetz/male nil)
          ::hf/render ref2) [:db/ident]}]}]))

(def !needle (atom ""))

(comment
  ((system exports (page-submissions ~@~(m/watch !needle))) prn u/pst)
  )

(def exports
  (vars into hf/rules sort q d/q *$* first !needle))
