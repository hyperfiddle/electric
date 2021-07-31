(ns user.webform
  "Web app tutorial, part 1 (no network)"
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            #_[hyperfiddle.q2 :refer [hf-nav hfql exports]]
            [hyperfiddle.rcf :refer [tests ! %]]
            [missionary.core :as m]
            [user.gender-shirt-size :refer [submissions genders shirt-sizes]]))

(p/defn render-shirt-size [v]
  [:select {:selected v}
   (p/for [x (p/$ shirt-sizes :dustingetz/male nil)]
     [:option x])])

(tests
  "reactive widget with query/view composition"
  (def !shirt-size (atom :mens-large))
  (def dispose (p/run (! (p/$ render-shirt-size ~(m/watch !shirt-size)))))
  % := [:select {:selected :mens-large}
        [[:option 3] [:option 4] [:option 5]]]
  (reset! !shirt-size :mens-medium)
  % := [:select {:selected :mens-medium}
        [[:option 3] [:option 4] [:option 5]]]
  ; dispose doesn't work with reactive-for
  #_(dispose))

(p/defn submissions-form [e]
  [:tr
   [:field ~(hf/nav e :dustingetz/email)]
   [:field (p/$ render-shirt-size ~(hf/nav e :dustingetz/shirt-size))]])

(p/defn submissions-table [needle]
  [:table
   (p/for [e (p/$ submissions needle)]
     (p/$ submissions-form e))])

(tests
  "reactive table with query/view composition"
  (def !needle (atom "alice"))
  (def dispose (p/run (! (p/$ submissions-table ~(m/watch !needle)))))
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

(def !select-needle (atom ""))
(def !query-needle (atom "alice"))

(p/defn typeahead-select [v options]
  (let [needle ~(m/watch !select-needle)]
    [:select {:selected v}
     (p/for [e (doto (p/$ options needle) prn)]
       [:option #_(name) e])]))

(tests
  (def dispose (p/run (! (p/$ typeahead-select
                              :dustingetz/womens-large
                              (p/fn [needle]
                                (p/$ shirt-sizes :dustingetz/male needle))
                              ))))
  % := [:select
        {:selected :dustingetz/womens-large}
        [[:option 3]
         [:option 4]
         [:option 5]]]
  #_(dispose)
  )

(p/defn render-shirt-size-with-select [v]
  (p/$ typeahead-select v (p/fn [needle]
                            (p/$ shirt-sizes :dustingetz/male needle))))

(p/defn submissions-form-with-select [e]
  [:tr
   [:field ~(hf/nav e :dustingetz/email)]
   [:field (p/$ render-shirt-size-with-select
                ~(hf/nav e :dustingetz/shirt-size))]])

(p/defn submissions-table-with-select [needle]
  [:table
   (p/for [e (p/$ submissions needle)]
     (p/$ submissions-form-with-select e))])

(tests
  (def !select-needle (atom ""))
  (def !query-needle (atom "alice"))
  (def dispose (p/run (! (p/$ submissions-table-with-select
                              ~(m/watch !query-needle)))))
  % := [:table
        [[:tr
          [:field "alice@example.com"]
          [:field [:select {:selected :dustingetz/womens-large}
                   [[:option 3] [:option 4] [:option 5]]]]]]]
  (reset! !select-needle "large")
  % := _
  (reset! !query-needle "bob")
  % := [:table
        [[:tr
          [:field "bob@example.com"]
          [:field [:select {:selected :dustingetz/mens-large}
                   [[:option 5]]]]]]]
  #_(dispose)
  )



