;; Same as `dustin.fiddle`, but all functions are `a -> m b`


(ns user.fiddle-effects
  (:require [datascript.core :as d]
            [hfdl.lang :refer [#?(:clj defnode) vars]]
            [dustin.dev]
            [hyperfiddle.api :refer [*$*]]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hfdl.lang :refer [defnode]])))

(def q (comp #(m/ap (m/? (m/via m/blk %))) d/q))

(defmacro thread [body]
  `(unquote (m/ap (m/? (m/via m/blk ~body)))))
; when lambdas work, thread will work for free because m/ap generates lambda

(defnode genders []
  (into [] ~(q '[:find [?e ...] :where [_ :dustingetz/gender ?e]] *$*)))

(defnode gender []
  (first (genders)))

(defn needle-match [v needle]
  (clojure.string/includes?
   (.toLowerCase (or (str v) ""))
   (.toLowerCase (or (str needle) ""))))

(def needle-rule
  '[[(geoffrey.fiddle-effects/needle-match ?v ?needle)
     [(str ?v) ?v']
     [(str ?needle) ?needle']
     #_[(.toLowerCase ?v')]
     #_[(.toLowerCase ?needle')]
     #_[(clojure.string/includes? ?v' ?needle')]
     [(clojure.string/includes? ?v' ?needle')]]])

(defnode ^{:db/cardinality :db.cardinality/many}
  shirt-sizes [gender needle]
  #_(println `(shirt-sizes ~gender ~needle))
  (m/via m/cpu
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
        *$* needle-rule gender (or needle "")))))

(defnode ^{:db/cardinality :db.cardinality/one} shirt-size [gender needle]
  (first (shirt-sizes gender needle)))

(defn submissions [& [needle]]
  (m/via m/cpu (sort
             (d/q '[:find [?e ...]
                    :in $ % ?needle
                    :where
                    [?e :dustingetz/email ?email]
                    (needle-match ?email ?needle)
                    #_[(dustin.fiddle/needle-match ?email ?needle)]]
                  *$* needle-rule (or needle "")))))

(defnode submission-details [eid] eid)

(defnode submission [needle]
  (first (submissions needle)))

(def exports (vars into needle-rule sort q *$* first))

;; ((hfdl.impl.trace/system (hfdl.impl.trace/debug sample (submission))) prn prn)
;; @sample
