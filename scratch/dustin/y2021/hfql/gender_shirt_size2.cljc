(ns dustin.gender-shirt-size2
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :refer [#?(:clj defnode) vars system]]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs
     (:require-macros
       [user.gender-shirt-size
        :refer [render-shirt-size render-form
                page-submission-detail page-submissions]])))

(defnode genders []
  (into [] (d/q '[:find [?e ...] :where [_ :person/gender ?e]] *$*)))

(defnode shirt-sizes [gender & [needle]]
  (sort
    (d/q
      '[:in $ % ?gender ?needle
        :find [?e ...]
        :where
        [?e :person/type :person/shirt-size]
        [?e :person/gender ?gender]
        [?e :db/ident ?ident]
        (hyperfiddle.api/needle-match ?ident ?needle)]
      *$* gender (or needle ""))))

(defnode submissions [& [needle]]
  (sort
    (d/q '[:find [?e ...]
           :in $ % ?needle
           :where
           [?e :person/email ?email]
           (hyperfiddle.api/needle-match ?email ?needle)]
         *$* (or needle ""))))

(defnode submission-detail [e] e)

(s/fdef submissions :args (s/cat :needle string?))
(s/fdef shirt-sizes :args (s/cat :gender keyword?, :needle string?))

(def app
  (hf/app
    [{(submissions _)
      [:db/id
       (:person/email ::hf/a (submission-detail %))
       {(:person/gender ::hf/options (genders)) [:db/ident]}
       {(:person/shirt-size ::hf/options (shirt-sizes gender _)) [:db/ident]}]}

     {(submission-detail _)
      [:db/id
       :person/email
       {(:person/gender ::hf/options (genders)) [:db/ident]}
       {(:person/shirt-size ::hf/options (shirt-sizes gender _)) [:db/ident]}]}]))


(def exports
  (vars into sort q d/q *$* first))
