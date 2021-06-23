(ns user.gender-shirt-size
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            [hfdl.lang :refer [#?(:clj defnode) vars system]]
            [hfdl.impl.util :as u]
            [hyperfiddle.api :as hf :refer [*$*]]
            [hyperfiddle.q2 :refer [hf-nav hfql exports]]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs
     (:require-macros
       [user.gender-shirt-size
        :refer [render-shirt-size render-form
                page-submission-detail page-submissions]])))

(def q (comp #(m/ap (m/? (m/via m/blk %))) d/q))

(defnode genders []
  (into [] ~(q '[:find [?e ...] :where [_ :dustingetz/gender ?e]] *$*)))

;(tests
;  (genders) := [:dustingetz/male :dustingetz/female] #_[male female])

(defnode gender []
  (first (genders)))

;(tests
;  (gender) := :dustingetz/male #_male)

(defnode shirt-sizes [gender & [needle]]
  (sort
    (d/q
      '[:in $ % ?gender ?needle
        :find [?e ...]
        :where
        [?e :dustingetz/type :dustingetz/shirt-size]
        [?e :dustingetz/gender ?gender]
        [?e :db/ident ?ident]
        (hyperfiddle.api/needle-match ?ident ?needle)]
      *$* hf/rules gender (or needle ""))))

(s/fdef shirt-sizes
        :args (s/alt :naked (s/cat :gender keyword? #_nat-int?)
                     :needle (s/cat :gender keyword?, :needle string?))
        :ret sequential?)

(defn submissions [& [needle]]
  ;(m/via m/cpu)
  (sort
    (d/q '[:find [?e ...]
           :in $ % ?needle
           :where
           [?e :dustingetz/email ?email]
           (hyperfiddle.api/needle-match ?email ?needle)]
         *$* hf/rules (or needle ""))))

(s/fdef submissions
        :args (s/alt :naked (s/cat) :needle (s/cat :needle string?))
        :ret sequential?)

;(tests
;  (submissions) := [alice bob charlie]
;  (submissions "example") := [alice bob charlie]
;  (submissions "b") := [bob])

(defnode render-shirt-size [v {::hf/keys [options]}]
  (dom/select {:selected v} (for [x options] (dom/option x))))

(defnode render-form [e]
  (dom/tr
    (dom/field ~@(hf-nav :dustingetz/email e))
    (dom/field (render-shirt-size ~@(hf-nav :dustingetz/shirt-size e)))))

(defnode page-submission-detail "" [e]
  (hfql
    ; how to link back? command bar and stack?
    [{e
      [:db/id
       :dustingetz/email
       {(:dustingetz/gender ::hf/options (genders)) [:db/ident]}
       {(:dustingetz/shirt-size ::hf/options (shirt-sizes gender)
          ::hf/render render-shirt-size) [:db/ident]}]}]))

(defnode page-submissions "" [needle]
  (hfql
    [{((submissions needle) ::hf/render render-form)
      [:db/id
       (:dustingetz/email ::hf/a page-submission-detail)
       {(:dustingetz/gender ::hf/options (genders)) [:db/ident]}
       {(:dustingetz/shirt-size ::hf/options (shirt-sizes gender)
          ::hf/render render-shirt-size) [:db/ident]}]}]))

(def !needle (atom ""))

(comment
  ((system exports (page-submissions ~@~(m/watch !needle))) prn u/pst)
  )

(def exports
  (vars into hf/rules sort q d/q *$* first !needle))
