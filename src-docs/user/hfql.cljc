(ns user.hfql
  (:require [hfdl.lang :as r :refer [defnode node]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.q2 :refer [hfql]]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [missionary.core :as m]
            [user.gender-shirt-size :refer [submissions genders shirt-sizes]]))

(defnode submission [needle] (first (submissions needle)))
(defnode gender [] (first (genders)))
(defnode shirt-size [gender & [needle]] (first (shirt-sizes gender needle)))

(defnode render-shirt-size [v]
  [:select {:selected v}
   (r/for [x (shirt-sizes :dustingetz/male nil)]
          [:option x])])

(defnode page-submission [needle]
  (hfql
    [{(submission needle)
      [:db/id
       :dustingetz/email
       (:dustingetz/shirt-size ::hf/render render-shirt-size)]}]))

(tests
  "cardinality one"
  (def !needle (atom "alice"))
  (def dispose (r/run (! (page ~(m/watch !needle)))))
  % := '{(user.hfql/submission "alice")
         {:db/id                 9
          :dustingetz/email      "alice@example.com",
          :dustingetz/shirt-size [:select
                                  {:selected :dustingetz/womens-large}
                                  [:option 3]
                                  [:option 4]
                                  [:option 5]]}}

  (reset! !needle "bob")
  % := '{(user.hfql/submission "bob")
         {:dustingetz/email "bob@example.com",
          :dustingetz/shirt-size [:select
                                  {:selected :dustingetz/mens-large}
                                  [:option 3]],
          :db/id 10}}
  (dispose))

(defnode page-submissions [needle]
  (hfql
    [{(submissions needle)
      [:db/id
       :dustingetz/email
       (:dustingetz/shirt-size ::hf/render render-shirt-size)]}]))

(tests
  "cardinality many"
  (def !needle (atom ""))
  (def dispose (r/run (! (page ~(m/watch !needle)))))
  % := '{(user.gender-shirt-size/submission "")
         [{:db/id                 9
           :dustingetz/email      "alice@example.com",
           :dustingetz/shirt-size [:select
                                   {:selected :dustingetz/womens-large}
                                   [:option 3]
                                   [:option 4]
                                   [:option 5]]}
          {:db/id                 10
           :dustingetz/email      "bob@example.com",
           :dustingetz/shirt-size [:select
                                   {:selected :dustingetz/mens-large}
                                   [:option 3]
                                   [:option 4]
                                   [:option 5]]}
          {:db/id                 11
           :dustingetz/email      "charlie@example.com",
           :dustingetz/shirt-size [:select
                                   {:selected :dustingetz/mens-large}
                                   [:option 3]
                                   [:option 4]
                                   [:option 5]]}]}

  (reset! !needle "bob")
  % := '{(user.gender-shirt-size/submission "bob")
         [{:dustingetz/email      "bob@example.com",
           :dustingetz/shirt-size [:select
                                   {:selected :dustingetz/mens-large}
                                   [:option 3]],
           :db/id                 10}]}
  (dispose))
