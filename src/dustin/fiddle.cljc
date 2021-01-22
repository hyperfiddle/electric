(ns dustin.fiddle
  #?(:cljs (:require-macros [minitest :refer [tests]]))
  (:require
    [clojure.spec.alpha :as s]
    [datascript.core :as d]
    ;#?(:clj [datomic.api :as d] :cljs [datascript.core :as d])
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [hyperfiddle.api :refer [*$*]]
    [minitest :refer [tests]]))


(defn genders []
  (d/q '[:find [?e ...]
         :where [_ :dustingetz/gender ?e]]
    *$*))

(tests
  (genders) := [:dustingetz/male :dustingetz/female] #_[male female])

(defn gender []
  (first (genders)))

(tests
  (gender) := :dustingetz/male #_male)

(defn needle-match [v needle]
  (clojure.string/includes?
    (.toLowerCase (or (str v) ""))
    (.toLowerCase (or (str needle) ""))))

(tests
  (needle-match "alice" "a") := true
  (needle-match "alice" "A") := true
  (needle-match "alice" "b") := false)

(defn needle-match' [v needle]
  (boolean
    (re-find
      #?(:clj (re-pattern (str "(?i)" needle)) :cljs (js/RegExp. needle "i"))
      v)))

(tests
  (needle-match' "alice" "a") := true
  (needle-match' "alice" "A") := true
  (needle-match' "alice" "b") := false)

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
      *$* needle-rule gender (or needle ""))))

(s/fdef shirt-sizes :ret sequential?)

(tests
  (shirt-sizes :dustingetz/male) := [m-sm m-md m-lg]
  (shirt-sizes :dustingetz/male "med") := [m-md]
  (shirt-sizes :dustingetz/male "sm") := [m-sm])

(defn ^{:db/cardinality :db.cardinality/one}
  shirt-size [gender]
  (first (shirt-sizes gender)))

(s/fdef shirt-size :ret (complement sequential?))

(tests
  (shirt-size :dustingetz/male) := m-sm
  ;(shirt-size male) := m-sm ; datascript issue?
  )

(defn submissions [& [needle]]
  (sort
    (d/q '[:find [?e ...]
           :in $ % ?needle
           :where
           [?e :dustingetz/email ?email]
           (needle-match ?email ?needle)
           #_[(dustin.fiddle/needle-match ?email ?needle)]]
      *$* needle-rule (or needle ""))))

(tests
  (submissions) := [alice bob charlie]
  (submissions "example") := [alice bob charlie]
  (submissions "b") := [bob])

(defn submission [& [needle]]
  (first (submissions needle)))

(tests
  (submission) := alice
  (submission "ali") := alice
  (submission "bob") := bob)

(def submissionS submissions)



;(def ast0 '[{(submission >needle) [{:dustingetz/gender
;                                    [:db/ident
;                                     {(shirt-size dustingetz/gender) [*]}]}]}
;            (gender)])
