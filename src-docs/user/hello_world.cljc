;(in-ns 'user.hello-world)
(ns user.hello-world
  (:require [hfdl.impl.sampler :refer [sampler!]]
            [hfdl.lang :refer [system dataflow vars]]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]
            [clojure.string :as str]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql]]
            ))

(defn run-dag!
  ([dag] (run-dag! {} dag))
  ([vars dag]
   ((system vars (fn [] (sampler! #(def sampler %) dag))) prn prn)
   sampler))

#?(:cljs
   (tests
     1 := 1

     @(run-dag! {} (shirt-size :dustingetz/male)) := 3
     @(run-dag! {} (shirt-sizes :dustingetz/male)) := [3 4 5]

     (def !needle (atom "alice"))
     (def sampler
       (run-dag! (vars str/upper-case)
                 (dataflow
                   (let [needle @(m/watch !needle)]
                     ~@(str/upper-case needle)))))

     @sampler := "ALICE"
     (reset! !needle "bob")
     @sampler := "BOB"

     ))

(defmacro reactive-for [& body])
(defmacro defnode [& body])

(defnode render-shirt-size [v]
  [:select {:selected @v}
   [:option ~@(shirt-size :dustingetz/male)]])

(defnode render-form [e]
  ~@[:tr
     [:field ~@@(hf-nav :dustingetz/email e)]
     [:field ~@@(render-shirt-size (hf-nav :dustingetz/shirt-size e))]])

#?(:cljs
   (tests
     (def !needle (atom "alice"))

     (def dag
       (dataflow
         (let [needle @(m/watch !needle)]
           [:table
            (reactive-for
              [e @(submissions needle)]
              (render-form e))])))

     ; inlined
     (def dag
       (dataflow
         (let [needle @(m/watch !needle)]
           [:table
            ~@(reactive-for [e @(submissions needle)]
                (let [x @(hf-nav :dustingetz/email ~e)
                      y @(hf-nav :dustingetz/shirt-size ~e)
                      z @(shirt-size :dustingetz/male)]
                  ~@[:tr #_:form
                     [:field (identity x)]
                     ; these two Z patterns have the same network characteristic
                     [:field [:select {:selected y} [:option z]]]
                     [:field [:select {:selected y} [:option ~@@(shirt-size :dustingetz/male)]]]]
                  ))])))

     @sampler
     := [:form
         [:field "alice@example.com"]
         [:field [:select {:selected :dustingetz/male}
                  [:option]]]]



     ))





#?(:cljs
   (tests

     (defn render-shirt-size-select [>v props]
       #?(:cljs
          (dataflow
            [:select {:selected @>v}
             [:option ~@(shirt-size :dustingetz/male)]])))

     (def !needle (atom "alice"))
     (def dag
       (dataflow
         (flatten-all
           ~@(hfql
               [{(submission needle)
                 [:db/id
                  :dustingetz/email
                  (:dustingetz/shirt-size ::hyperfiddle.api/render render-shirt-size-select)]}]))))

     (def sampler (run-dag! (vars) dag))
     @sampler
     := {'(geoffrey.fiddle-effects/submission needle)
         {:dustingetz/email      "bob@example.com"
          :dustingetz/shirt-size [:select {:selected :dustingetz/male}
                                  [:option]]}}

     (reset! !needle "bob")
     @sampler
     := {'(geoffrey.fiddle-effects/submission needle)
         {:dustingetz/email      "alice@example.com"
          :dustingetz/shirt-size [:select {:selected :dustingetz/male}
                                  [:option]]}}

     ;(run-dag! (vars id-as-string str join-1)
     ;          (dataflow (-> (hfql {(submissions "") [:db/id]})
     ;                        (deref)
     ;                        (get '(geoffrey.fiddle-effects/submissions ""))
     ;                        (deref)
     ;                        (first)
     ;                        (:db/id)
     ;                        (deref))))



     ))
