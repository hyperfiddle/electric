(ns dustin.hypermedia)

(in-ns 'user.hello-world)

(comment

  (defn submission-master [first second last]
    #?(:clj
       (->>
         (d/q '[:in $ ?needle
                :find [?e ...]
                :where
                [?e :dustingetz/email]
                [(get-else $ ?e :dustingetz/email "") ?email]
                [(.toLowerCase ?email) ?email2]
                [(.toLowerCase ?needle) ?needle2]
                [(clojure.string/includes? ?email2 ?needle2)]]
           (hf/get-db "$") (or needle ""))
         ;(sort-by :dustingetz/email)
         (take 10)
         (doall))))

  (s/fdef submission-master :args (s/cat :first string? :second string? :last number?)
    :ret ...)

  (defn shirt-sizes [gender]
    #?(:clj
       (if gender
         (doall
           (d/q '[:in $ ?gender
                  :find
                  (pull ?e [:db/id *])
                  :where
                  [?e :dustingetz/type :dustingetz/shirt-size]
                  [?e :dustingetz/gender ?gender]]
             (hf/get-db "$") gender))
         ())))

  (defn submission-detail [e] ...)

  (tests

    (submissions "alice") := [123, 124, 125]
    (shirt-sizes female-dbid) := [17592186045436 17592186045437 17592186045438]

    (hf-pull [{(submissions "")
               [:dustingetz/email
                :dustingetz/email1
                {:dustingetz/shirt-size [:db/ident]}
                {:dustingetz/gender [#_:db/ident (:db/ident %)
                                     #_shirt-sizes #_(shirt-sizes %) (shirt-sizes dustingetz/gender)

                                     {(foo gender) [:dustingetz/gender]}
                                     ]}]}
              {(genders) [:db/ident]}])


    := '[{(genders) [#:db{:ident :dustingetz/male}
                     #:db{:ident :dustingetz/female}]}
         ;{(genders) [#:db{:ident :dustingetz/male :db/id 1} #:db{:ident :dustingetz/female :db/id 2}]}
         {(submissions needle) :as submission
                               [{:dustingetz/email      "alice@example.com",
                                 :dustingetz/email1     "foo"
                                 :dustingetz/shirt-size {:db/ident :dustingetz/womens-large}
                                 :dustingetz/gender     {:db/ident :dustingetz/male,
                                                         (shirt-sizes dustingetz/gender needle:submission)
                                                                   [#:db{:ident :dustingetz/womens-small}
                                                                    #:db{:ident :dustingetz/womens-medium}
                                                                    #:db{:ident :dustingetz/womens-large}]}}
                                {:dustingetz/email      "bob@example.com",
                                 :dustingetz/email1     "foo"
                                 :dustingetz/shirt-size {:db/ident :dustingetz/womens-large}
                                 :dustingetz/gender     {:db/ident :dustingetz/male,
                                                         (shirt-sizes dustingetz/gender needle:submission)
                                                                   [#:db{:ident :dustingetz/mens-small}
                                                                    #:db{:ident :dustingetz/mens-medium}
                                                                    #:db{:ident :dustingetz/mens-large}]}}
                                {:dustingetz/email      "charlie@example.com",
                                 :dustingetz/email1     "foo"
                                 :dustingetz/shirt-size {:db/ident :dustingetz/womens-large}
                                 :dustingetz/gender     {:db/ident :dustingetz/male,
                                                         (shirt-sizes dustingetz/gender needle:submission)
                                                                   [#:db{:ident :dustingetz/mens-small}
                                                                    #:db{:ident :dustingetz/mens-medium}
                                                                    #:db{:ident :dustingetz/mens-large}]}}]}]

    := '[{(shirt-sizes :dustingetz/female "lar")
                    [#:db{:ident :dustingetz/womens-small}
                     #:db{:ident :dustingetz/womens-medium}
                     #:db{:ident :dustingetz/womens-large}]
          (shirt-sizes :dustingetz/male "")
                    [#:db{:ident :dustingetz/mens-small}
                     #:db{:ident :dustingetz/mens-medium}
                     #:db{:ident :dustingetz/mens-large}]
          (shirt-sizes :dustingetz/male "")
                    [#:db{:ident :dustingetz/mens-small}
                     #:db{:ident :dustingetz/mens-medium}
                     #:db{:ident :dustingetz/mens-large}]

          (genders) [#:db{:ident :dustingetz/male} #:db{:ident :dustingetz/female}]}
         {(submissions needle) :as submission
                               [{:dustingetz/email      "alice@example.com",
                                 :dustingetz/email1     "foo"
                                 :dustingetz/shirt-size {:db/ident :dustingetz/womens-large}
                                 :dustingetz/gender     {:db/ident                       :dustingetz/male,
                                                         (shirt-sizes dustingetz/gender needle:submission)
                                                                                         ~(shirt-sizes :dustingetz/female "wom")}}
                                {:dustingetz/email      "bob@example.com",
                                 :dustingetz/email1     "foo"
                                 :dustingetz/shirt-size {:db/ident :dustingetz/mens-large}
                                 :dustingetz/gender     {:db/ident :dustingetz/male,
                                                         (shirt-sizes dustingetz/gender needle:submission)
                                                                   ~(shirt-sizes :dustingetz/male "")}}
                                {:dustingetz/email      "charlie@example.com",
                                 :dustingetz/email1     "foo"
                                 :dustingetz/shirt-size {:db/ident :dustingetz/mens-large}
                                 :dustingetz/gender     {:db/ident :dustingetz/male,
                                                         (shirt-sizes dustingetz/gender needle:submission)
                                                                   ~(shirt-sizes :dustingetz/male "")}}]}]


    ; view progressive enhancement
    (defmethod hf/render :dustingetz/gender [val ctx props]
      [radio-options val ctx {:options '(genders)}])

    (defmethod hf/render :dustingetz/shirt-size [val ctx props] ; womens-large
      [select-options val ctx {:options '(shirt-sizes dustingetz/gender)}])

    (defmethod hf/render '(genders) [val ctx props] nil)
    (defmethod hf/render '(shirt-sizes dustingetz/gender) [val ctx props] nil)

    )

  (defn submission-master [first second last] ...)
  (defn genders [] ...)
  (defn submission-detail [] ...)
  (defn shirt-sizes [gender] ...)

  ; api
  (def route1 '(user.hello-world/submission-detail
                 [:dustingetz/email "alice@example.com"]
                 1 4))

  ; native database reads
  (fn r2d2-brain []
    (hfql {(user.hello-world/submission-detail
             [:dustingetz/email "alice@example.com"]) [*]}))

  (-> (hf-resolve route2)
    I/sequence-some
    (bindI I/sequence-some)
    #_(bindI I/sequence-some)
    capI)

  (def App
    {`submission-master [{(submissions *$* "alice" 1 2)
                          [:dustingetz/email ::hf/anchor '(submission-detail submission-id)
                           {:dustingetz/shirt-size [:db/ident]}
                           {:dustingetz/gender [:db/ident #_(:db/ident %)
                                                shirt-sizes #_(shirt-sizes %) #_(shirt-sizes dustingetz/gender)]}
                           #_{:dustingetz/gender
                              [:db/ident
                               {(shirt-sizes dustingetz/gender)
                                [:db/ident]}]}]}
                         {(genders) [:db/ident]}]
     `submission-detail [:db/id
                         :dustingetz/email
                         {:dustingetz/gender [:db/ident :dustingetz/type]}
                         {:dustingetz/shirt-size [:db/ident
                                                  (shirt-sizes dustingetz/gender)]}
                         ;{(genders) [:db/ident :dustingetz/type]}
                         (genders)]
     `genders           [:db/ident :dustingetz/type]
     `shirt-sizes       [:db/id :db/ident :dustingetz/type :dustingetz/gender]

     })

  )

