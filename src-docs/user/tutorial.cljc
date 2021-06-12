(ns user.tutorial
  (:require [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode vars system]]
            [missionary.core :as m]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql exports]]
            [hfdl.impl.util :as u]))

(defnode hello [] "hello-world")

(defnode num? [a] (number? a))

(defnode plus [a b] (+ a b))

(defnode render-shirt-size [v]
  [:select {:selected v}
   [:option (shirt-size :dustingetz/male nil)]])

(defnode render-shirt-size2 [v]
  [:select {:selected v}
   [:option ~@(shirt-size :dustingetz/male nil)]])

(defnode render-form [e]
  [:tr
   [:field ~@(hf-nav :dustingetz/email e)]
   [:field (render-shirt-size2 ~@(hf-nav :dustingetz/shirt-size e))]])

(defnode hello-world [needle]
  [:table
   ~@(h/for [e (submissions needle)]
       ~@(render-form e))])

(defnode page-submission-detail [needle]
  (hfql
    [{(submission needle)
      [:db/id
       :dustingetz/email
       (:dustingetz/shirt-size :hyperfiddle.api/render render-shirt-size2)]}]))

(comment
  "hello world"
  (def !result (atom nil))
  ((system exports (reset! !result (hello))) prn u/pst)
  @!result := "hello-world"
  ; system is still running for effect
  )

; dag is a composite with both local and remote part. remote will be sent to server for evaluation

(comment
  "node args feel like values"
  (def !result (atom nil))
  ((system exports (reset! !result (num? 1))) prn u/pst)
  @!result := true
  )

(comment
  "node args are actually flows"
  (def !input (atom 2))
  (def !result (atom nil))
  ((system exports (reset! !result (plus 1 ~(m/watch !input)))) prn u/pst)
  @!result := 3
  (swap! !input inc)
  @!result := 4
  )

(comment
  "simple effects + composing view and query in one process"
  (def !shirt-size (atom :mens-large))
  (def !result (atom nil))
  ((system exports (reset! !result (render-shirt-size ~(m/watch !shirt-size)))) prn u/pst)
  @!result := [:select {:selected :mens-large} [:option 3]]
  (reset! !shirt-size :mens-medium)
  @!result := [:select {:selected :mens-medium} [:option 3]]
  )

(comment
  "simple effects + composing view and query with transfer"
  ; this test passes in both directions since it is datascript and no host interop

  (def !shirt-size (atom :mens-large))
  (def !result (atom nil))
  ((system exports (reset! !result (render-shirt-size2 ~(m/watch !shirt-size)))) prn u/pst)

  @!result := [:select {:selected :mens-large} [:option 3]]
  (reset! !shirt-size :mens-medium)
  @!result := [:select {:selected :mens-medium} [:option 3]]
  )

; We want to pretend view is client and query is server
; So let's run this test from CLJS browser repl

; host interop is forbidden in remote blocks and not sure about lambdas (arguably this is interop)

(comment
  "crud"

  (def !needle (atom "alice"))
  (def !result (atom nil))
  ((system exports (reset! !result (hello-world ~(m/watch !needle)))) prn u/pst)

  @!result :=
  [:table
   [[:tr
     [:field "alice@example.com"]
     [:field [:select {:selected :dustingetz/womens-large}
              [:option 3]]]]]]

  (reset! !needle "bob")
  ; should target a point update to the :tr

  @!result :=
  [:table
   [[:tr
     [:field "bob@example.com"]
     [:field [:select {:selected :dustingetz/mens-large}
              [:option 5]]]]]]

  )


(comment
  "hfql"

  (def !needle (atom "alice"))
  (def !result (atom nil))
  ((system exports (reset! !result ~@(page-submission-detail ~@~(m/watch !needle)))) prn u/pst)

  @!result :=
  {(geoffrey.fiddle-effects/submission "alice")
   {:dustingetz/email "alice@example.com",
    :dustingetz/shirt-size [:select
                            {:selected :dustingetz/womens-large}
                            [:option 3]],
    :db/id 9}}

  (reset! !needle "bob")

  @!result :=
  {(geoffrey.fiddle-effects/submission "bob")
   {:dustingetz/email "bob@example.com",
    :dustingetz/shirt-size [:select
                            {:selected :dustingetz/mens-large}
                            [:option 3]],
    :db/id 10}}

  )

(defnode page-submissions [needle]
  (hfql
    [{(submissions needle)
      [:db/id
       :dustingetz/email
       (:dustingetz/shirt-size :hyperfiddle.api/render render-shirt-size2)]}]))

(comment
  "hfql cardinality many"

  (def !needle (atom ""))
  (def !result (atom nil))
  ((system exports (reset! !result ~@(page-submissions ~@~(m/watch !needle)))) prn u/pst)

  @!result :=
  {(geoffrey.fiddle-effects/submissions "")
   [{:dustingetz/email      "alice@example.com",
     :dustingetz/shirt-size [:select
                             {:selected :dustingetz/womens-large}
                             [:option 3]],
     :db/id                 9}
    {:dustingetz/email      "bob@example.com",
     :dustingetz/shirt-size [:select
                             {:selected :dustingetz/mens-large}
                             [:option 3]],
     :db/id                 10}
    {:dustingetz/email      "charlie@example.com",
     :dustingetz/shirt-size [:select
                             {:selected :dustingetz/mens-medium}
                             [:option 3]],
     :db/id                 11}]}

  (reset! !needle "bob")                                    ; broken test

  @!result :=
  {(geoffrey.fiddle-effects/submissions "bob")
   [{:dustingetz/email      "bob@example.com",
     :dustingetz/shirt-size [:select
                             {:selected :dustingetz/mens-large}
                             [:option 3]],
     :db/id                 10}]}

  )

; unquote, deref, calls to other defnodes, clojure interop, transfer
; unquote is for introducing flow transformers – a special form

; todo
;(tests
;  (defnode if2 ...))
