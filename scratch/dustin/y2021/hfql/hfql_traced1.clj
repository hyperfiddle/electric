(ns dustin.hfql.hfql-traced1
  (:require [dustin.fiddle :refer [submission gender shirt-size submission-details]]
            [geoffrey.hfql.links :refer [hfql]]
            [hfdl.lang :refer [dataflow]]
            [hfdl.impl.trace :refer [system debug]]
            [hyperfiddle.api :as hf]
            [minitest :refer [tests]]
            [missionary.core :as m]
            [datomic.api :as d]))

(declare page-submission-details)

(defn page-submissions [needle]
  (dataflow
    (hfql [{(submission @needle)
            [(:db/id ::hf/a (page-submission-details %))
             :dustingetz/email
             {:dustingetz/gender
              [:db/ident
               {(shirt-size dustingetz/gender) [:db/ident]}]}]}
           {(gender) [:db/ident]}])))

(defn page-submission-details [eid]
  (dataflow
    (hfql [{(submission-details eid) [:db/id
                                      :dustingetz/email
                                      :dustingetz/shirt-size
                                      {:dustingetz/gender [:db/ident {(shirt-size dustingetz/gender) [:db/ident]}]}]}
           {(gender) [:db/ident]}])))

(tests
  (tests
    (def !needle (atom "a"))
    (def dag (dataflow (keyword (deref (m/watch !needle)))))
    ((system (debug sampler dag)) prn prn)
    @sampler := :a)

  (tests
    (def !needle (atom "bob"))
    (def dag (dataflow (submission (deref (m/watch !needle)))))
    ((system (debug sampler dag)) prn prn)
    @sampler := 10)

  (tests
    (def !needle (atom "bob"))
    (def dag (dataflow (let [needle @(m/watch !needle)]
                         (hfql {(submission needle) [:db/id :dustingetz/email]}))))
    ((system (debug sampler dag)) prn prn)
    @sampler := {'(dustin.fiddle/submission needle) {:db/id 10 :dustingetz/email "bob@example.com"}}
    @sampler := {'(dustin.fiddle/submission needle) {:db/id 10 :dustingetz/email "bob@example.com"}}

    (reset! !needle "alice")
    @sampler := {'(dustin.fiddle/submission needle) {:db/id 9 :dustingetz/email "alice@example.com"}}
    )


  (tests
    "hf/render"
    (def !needle (atom "bob"))
    (defn render-email [v props] (dataflow [:div.email v]))
    (def dag (dataflow (let [needle @(m/watch !needle)]
                         (hfql {(submission needle) [:db/id (:dustingetz/email ::hf/render render-email)]}))))
    ((system (debug sampler dag)) prn prn)
    @sampler := {'(dustin.fiddle/submission needle) {:db/id 10 :dustingetz/email [:div.email "bob@example.com"]}}

    (reset! !needle "alice")
    @sampler := {'(dustin.fiddle/submission needle) {:db/id 9 :dustingetz/email [:div.email "alice@example.com"]}}
    )

  (tests
    "hfql with remote"
    (def !needle (atom "bob"))
    (def dag (dataflow (let [needle @(m/watch !needle)]
                         ~@(hfql {(submission needle) [:db/id]}))))
    ((system (debug sampler2 dag)) prn prn)
    ; network activity
    @sampler2 := {'(dustin.fiddle/submission needle) {:db/id 10}}
    )

  (tests
    "all queries in second peer"
    (def !needle (atom "bob"))

    (defn render-email [v {:keys [alice]}]
      (dataflow
        (let [alice (:dustingetz/email (datascript.core/entity hf/*$* alice))]
          ~@[:div.render
             [:div.me v]
             [:div.alice alice]])))

    (defn render-pre! [v]
      (m/ap (prn (m/?? v))))

    (def dag (dataflow (let [needle @(m/watch !needle)]
                         @(render-pre! ~~@(hfql {(submission needle) [:db/id (:dustingetz/email ::hf/render render-email :alice 9)]})))))
    ((system (debug sampler1 dag)) prn prn)
    @sampler1 := nil)


  (tests
    (s/fdef shirt-size :args (s/cat :gender ref? :needle string?))

    (defn shirt-size [gender & [needle]]
      ... (or needle "") ...)

    (defn thing [needle]
      (dataflow
        (remote
          (hfql {(submissions needle)
                 [:db/id
                  {:dustingetz/gender [:db/ident]}
                  {(:dustingetz/shirt-sizes ::hf/options (shirt-size gender _))
                   [:db/ident]}]}))))

    ;(def !needle (atom "bob"))
    (def dag (dataflow (let [needle @(m/watch !needle)]
                         (remote (hfql {(submission _)
                                        [:db/id
                                         {:dustingetz/gender [:db/ident]}
                                         {(:dustingetz/shirt-sizes ::hf/options (shirt-size gender _))
                                          [:db/ident]}]})))))
    ((system (debug sampler2 dag)) prn prn)
    ; network activity
    @sampler2 := {'(dustin.fiddle/submission needle) {:db/id 10 :dustingetz/email [:div.email "bob@example.com"]}}


    )



  (tests


    (defn codemirror [vs props]
      (binding []
        (for [v vs]
          (render))))


    (hfql
      {((submission needle) ::hf/render codemirror)
       [:db/id
        (:dustingetz/email ::hf/render codemirror-string-input)
        {(:dustingetz/gender ::hf/options (genders)) [:db/ident]}]}
      ))

  )
