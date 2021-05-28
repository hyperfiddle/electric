;; Same as `dustin.fiddle`, but all functions are `a -> m b`


(ns geoffrey.fiddle-effects
  (:require [datascript.core :as d]
            [hfdl.lang :refer [#?(:clj vars)]]
            [dustin.dev]
            [hyperfiddle.api :refer [*$*]])
  #?(:cljs (:require-macros [hfdl.lang :refer [dataflow]])))

(defn genders []
  #?(:cljs
     (dataflow
       (into []
         (d/q '[:find [?e ...]
                :where [_ :dustingetz/gender ?e]]
           *$*)))))

(defn gender []
  #?(:cljs
     (dataflow (first @(genders)))))

(defn needle-match [v needle]
  (clojure.string/includes?
   (.toLowerCase (or (str v) ""))
   (.toLowerCase (or (str needle) ""))))

(def needle-rule
  '[[(needle-match ?v ?needle)
     [(str ?v) ?v']
     [(str ?needle) ?needle']
     #_[(.toLowerCase ?v')]
     #_[(.toLowerCase ?needle')]
     #_[(clojure.string/includes? ?v' ?needle')]
     [(clojure.string/includes? ?v' ?needle')]]])

(defn ^{:db/cardinality :db.cardinality/many}
  shirt-sizes [gender & [needle]]
  #_(println `(shirt-sizes ~gender ~needle))
  #?(:cljs
     (dataflow
       (sort
         (d/q
           '[:in $ % ?gender ?needle
             :find [?e ...]
             :where
             [?e :dustingetz/type :dustingetz/shirt-size]
             [?e :dustingetz/gender ?gender]
             [?e :db/ident ?ident]
             (needle-match ?ident ?needle)
             #_[(dustin.fiddle/needle-match ?ident ?needle)]]
           *$* needle-rule gender (or needle ""))))))

(defn ^{:db/cardinality :db.cardinality/one}
  shirt-size [gender]
  #?(:cljs (dataflow (first @(shirt-sizes gender)))))

(defn submissions [& [needle]]
  #?(:cljs
     (dataflow
       (sort
         (d/q '[:find [?e ...]
                :in $ % ?needle
                :where
                [?e :dustingetz/email ?email]
                (needle-match ?email ?needle)
                #_[(dustin.fiddle/needle-match ?email ?needle)]]
           *$* needle-rule (or needle ""))))))

(defn submission-details [eid]
  #?(:cljs (dataflow eid)))

(defn submission [& [needle]]
  #?(:cljs (dataflow (first @(submissions needle)))))

(def exports
  #?(:clj (vars genders into gender needle-rule shirt-sizes sort d/q *$* first
            shirt-size submissions submission-details submission)))

;; ((hfdl.impl.trace/system (hfdl.impl.trace/debug sample (submission))) prn prn)
;; @sample
