(ns dustin.hfdl.lang
  (:require [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql]]
            ))

(defmacro rfor [bindings & body])
(defmacro defnode [s args & body])
(defmacro dag [& body])
(defmacro vars [& _])
(defn peer [boot read write])
(defn eval [dag resolver])
(defmacro run-task [f & args] `(unquote (m/ap (m/? (~f ~@args)))))
(def ! run-task)
(defmacro call-node [dag & args] "helper function for REPL only, wraps main")

(tests
  "hello world"
  ; both clj and cljs
  (defnode printer [] (clojure.core/print "hello-world"))
  (def sampler (call-node printer))
  @sampler
  ; hello-world
  := nil)

; dag is a composite with both local and remote part. remote will be sent to server for evaluation

(tests
  (defnode foo [a b] (type a))
  (def sampler (call-node foo 1 2))
  @sampler := number)

(tests
  "incremental maintenance"
  (defnode foo [a b] (+ a b))
  (def !x (atom 2))
  (def sampler (call-node foo 1 ~(m/watch !x)))
  @sampler := 3
  (swap! !x inc)
  @sampler := 4)

(defnode render-shirt-size [v]
  [:select {:selected v}
   [:option (run-task shirt-size :dustingetz/male)]])

(tests
  "simple effects + composing view and query in one process"
  (def !x (atom :mens-large))
  (def sampler (call-node render-shirt-size ~(m/watch !x)))
  @sampler := [:select {:selected :mens-large} [:option :mens-large]]
  (reset! !x :mens-medium)
  @sampler := [:select {:selected :mens-medium} [:option :mens-medium]])

(defnode render-shirt-size2 [v]
  [:select {:selected v}
   [:option ~@(run-task shirt-size :dustingetz/male (do v nil))]])

(tests
  "simple effects + composing view and query in one process"
  ; this test passes in both directions since it is datascript and no host interop
  (def !x (atom :mens-large))
  (def sampler (call-node render-shirt-size2 ~(m/watch !x)))
  @sampler := [:select {:selected :mens-large} [:option :mens-large]]
  (reset! !x :mens-medium)
  @sampler := [:select {:selected :mens-medium} [:option :mens-medium]])

; We want to pretend view is client and query is sever
; So let's run this test from CLJS browser repl
;


; host interop is forbidden in remote blocks and not sure about lambdas (arguably this is interop)

(defnode render-form [e]
  [:tr
     [:field ~@(! hf-nav :dustingetz/email e)]
     [:field ~@(render-shirt-size2 (! hf-nav :dustingetz/shirt-size e))]])

(defnode hello-world [needle]
  [:table
   ~@(rfor [e (! submissions needle)]
         ~@(render-form e))])

(tests
  "crud"
  (def !needle (atom "alice"))
  (def sampler (call-node hello-world ~(m/watch !needle)))
  @sampler
  := [:form
      [:field "alice@example.com"]
      [:field [:select {:selected :dustingetz/male}
               [:option]]]]

  (reset! !x "bob")
  @sampler := _)

(defnode page-submission-detail [needle]
  ~@(hfql
      [{(submission needle)
        [:db/id
         :dustingetz/email
         (:dustingetz/shirt-size ::hyperfiddle.api/render render-shirt-size2)]}]))

(tests
  "hfql"

  (def !needle (atom "alice"))
  (def sampler (call-node page-submission-detail ~(m/watch !needle)))
  @sampler
  := {'(geoffrey.fiddle-effects/submission needle)
      {:dustingetz/email      "alice@example.com"
       :dustingetz/shirt-size [:select {:selected :dustingetz/male}
                               [:option]]}}

  (reset! !x "bob")
  @sampler := _
  )

; unquote, deref, calls to other defnodes, clojure interop, transfer
; unquote is for introducing flow transformers – a special form

; todo
;(tests
;  (defnode if2 ...))
