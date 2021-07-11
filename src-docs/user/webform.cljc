(ns user.webform
  "Web app tutorial, part 1 (no network)"
  (:require [hfdl.lang :as h :refer [defnode]]
            [hfdl.impl.util :as u]
            [hyperfiddle.api :as hf]
            [hyperfiddle.q2 :refer [hf-nav hfql exports]]
            [hyperfiddle.rcf :refer [tests ! %]]
            [missionary.core :as m]
            [user.gender-shirt-size :refer [submissions genders shirt-sizes]]))

(defnode render-shirt-size [v]
  [:select {:selected v}
   (h/for [x (shirt-sizes :dustingetz/male nil)]
     [:option x])])

(tests
  "reactive widget with query/view composition"
  (def !shirt-size (atom :mens-large))
  (def dispose (r/run (! (render-shirt-size ~(m/watch !shirt-size)))))
  % := [:select {:selected :mens-large} [[:option 3] [:option 4] [:option 5]]]
  (reset! !shirt-size :mens-medium)
  % := [:select {:selected :mens-medium} [[:option 3] [:option 4] [:option 5]]]
  (dispose))

(defnode submissions-form [e]
  [:tr
   [:field (hf-nav :dustingetz/email e)]
   [:field (render-shirt-size (hf-nav :dustingetz/shirt-size e))]])

(defnode submissions-table [needle]
  [:table
   (h/for [e (submissions needle)]
     (submissions-form e))])

(tests
  "reactive table with query/view composition"
  (def !needle (atom "alice"))
  (def dispose (r/run (! (submissions-table ~(m/watch !needle)))))
  % := [:table
        [[:tr
          [:field "alice@example.com"]
          [:field [:select {:selected :dustingetz/womens-large}
                   [[:option 3] [:option 4] [:option 5]]]]]]]

  (reset! !needle "bob")
  % := [:table
        [[:tr
          [:field "bob@example.com"]
          [:field [:select {:selected :dustingetz/mens-large}
                   [[:option 3] [:option 4] [:option 5]]]]]]])
