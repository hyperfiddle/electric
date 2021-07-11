(ns user.webform-distributed
  (:require [hfdl.lang :as r :refer [defnode]]
            [hyperfiddle.rcf :refer [tests ! %]]
            [hyperfiddle.q2 :refer [hf-nav hfql]]
            [missionary.core :as m]
            [user.gender-shirt-size :refer [submissions genders shirt-sizes]])
  #?(:cljs (:require-macros
             [hfdl.lang :refer [defnode]]
             [user.webform-distributed
              :refer [render-shirt-size render-form page-submission-detail page-submissions]])))

; dag is a composite with both local and remote part.
; remote will be sent to server for evaluation
; We want to pretend view is client and query is server
; So let's run this test from CLJS browser repl
; host interop is forbidden in remote blocks and not sure about lambdas (arguably this is interop)

(defnode render-shirt-size [v]
  ~@[:select {:selected v}
     ~@(r/for [x (shirt-sizes :dustingetz/male nil)]
         ~@[:option x])])

(comment
  "distributed widget with query/view composition"
  ; this test can pass in both directions since it is datascript and no host interop
  (def !shirt-size (atom :mens-large))
  (def exports (merge hyperfiddle.q2/exports (r/vars render-shirt-size !shirt-size)))
  (def dispose (r/run2 exports (! ~@(render-shirt-size ~@~(m/watch !shirt-size)))))
  % := [:select {:selected :mens-large} [[:option 3] [:option 4] [:option 5]]]
  (reset! !shirt-size :mens-medium)
  % := [:select {:selected :mens-medium} [[:option 3] [:option 4] [:option 5]]]
  (dispose))

(defnode submissions-form [e]
  ~@[:tr
     [:field ~@(hf-nav :dustingetz/email e)]
     [:field ~@(render-shirt-size (hf-nav :dustingetz/shirt-size e))]])

(defnode submissions-table [needle]
  ~@[:table
     ~@(r/for [e (submissions needle)]
         (submissions-form e))])

(comment
  "distributed table with query/view composition"
  (def !needle (atom "alice"))
  (def exports (merge hyperfiddle.q2/exports
                      (r/vars render-shirt-size submissions-form submissions-table !needle)))
  (def dispose (r/run2 exports (! ~@(submissions-table ~@~(m/watch !needle)))))
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
