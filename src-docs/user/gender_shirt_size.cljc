(ns user.gender-shirt-size
  (:require [clojure.spec.alpha :as s]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [! % tests]])
  #?(:cljs (:require-macros [user.gender-shirt-size :refer [genders gender shirt-sizes submissions submission emails sub-profile]])))


(s/fdef genders :ret (s/coll-of number?))
(p/defn genders []
  (into [] ~ (hf/q '[:find [?e ...] :where [_ :dustingetz/gender ?e]])))

(tests
  (def dispose (p/run (! (p/$ genders))))
  % := [:dustingetz/male :dustingetz/female]
  (dispose))

(p/defn gender []
  (first (p/$ genders)))


(tests
  (def dispose (p/run (! (p/$ gender))))
  % := :dustingetz/male
  (dispose))

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?)
        :ret (s/coll-of number?))

(p/defn shirt-sizes [gender needle]
  (sort
   ~(hf/q '[:in $ % ?gender ?needle
            :find [?e ...]
            :where
            [?e :dustingetz/type :dustingetz/shirt-size]
            [?e :dustingetz/gender ?gender]
            [?e :db/ident ?ident]
            (hyperfiddle.api/needle-match ?ident ?needle)]
          hf/rules gender (or needle ""))))

(p/defn emails [needle]
  ~(hf/q '[:in $ % ?needle
           :find [?e ...]
           :where
           [?e :dustingetz/email ?email]
           (hyperfiddle.api/needle-match ?email ?needle)]
         hf/rules (or needle "")))

(s/fdef emails :args (s/cat :needle string?)
        :ret (s/coll-of string?))

(p/defn submissions [needle]
  (sort
    ~(hf/q '[:find [?e ...]
          :in $ % ?needle
          :where
          [?e :dustingetz/email ?email]
          (hyperfiddle.api/needle-match ?email ?needle)]
       hf/rules (or needle ""))))

(s/fdef submissions :args (s/cat :needle string?)
        :ret (s/coll-of (s/keys :req [:dustingetz/email
                                      :dustingetz/email1
                                      :dustingetz/gender
                                      :dustingetz/shirt-size])))


(s/fdef submission :ret number?)
(p/defn submission [needle] (first (p/$ submissions needle)))


(s/fdef sub-profile :args (s/cat :sub any?) :ret any?)
(p/defn sub-profile [sub] ~(hf/nav sub :db/id))

(tests
  (hfdl.lang/run (! (p/$ user.gender-shirt-size/submission "")))
  %)

(tests
  (def dispose
    (p/run
      (! (p/$ submissions ""))
      (! (p/$ submissions "example"))
      (! (p/$ submissions "b"))))
  % := [9 10 11]
  % := [9 10 11]
  % := [10]
  (dispose))
