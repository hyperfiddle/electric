(comment

  (p/defn gender [e])


  {(submissions .)
   [:dustingetz/email
    {(:dustingetz/gender {::hf/options      (genders)
                          ::hf/option-label :db/ident})
     [(:db/ident {::hf/as gender})]}

    {^{::hf/options      (genders)
       ::hf/option-label :db/ident}
     :dustingetz/gender
     [:db/ident]}

    {((gender) {::hf/options      (genders)
                ::hf/option-label :db/ident})
     [:db/ident]}

    {^{::hf/options      (genders)
       ::hf/option-label :db/ident}
     (gender)
     [:db/ident]}

    {(:dustingetz/shirt-size {::hf/options      (shirt-sizes gender .)
                              ::hf/option-label :db/ident})
     [:db/ident]}]}

  )

; links produce routes
; routes consume links (?)

(comment
  (ns main
    (:require
      [suber.web.user :as-alias user]
      [_ :as-alias _]))

  (s/fdef sub-display :args (s/cat :dustin.user/sub (s/and some? ref?)))
  (s/fdef all-tags :args (s/cat :needle string?))
  (s/fdef sub-locations :args (s/cat :sub ref?))
  (s/fdef school-requested-block :args (s/cat
                                         :sub (s/and some? ref?)
                                         :school (s/and some? ref?)
                                         :block-reason (s/and some? ref?)
                                         :block-mode (s/and some? keyword?)
                                         :penalize boolean?))

  (hfql
    {(sub-display {::sub [suber-name]})
     [suber-name                                            ; label = "suber-name"
      #link suber-name                                      ; label = "suber-name"
      (suber-name %)                                        ; label = "(suber-name %)"
      (suber-name user/sub)                                 ; label = "(suber-name user/sub)"

      ^{::hf/options (all-tags ^{::hf/default (or needle "")} needle)
        ::hf/render  ...}
      (sub-tags sub)

      ^{::hf/options (all-locations ^{::hf/default (or needle "")} needle)}
      sub-locations
      ;(sub-locations . {::hf/options (all-locations [:needle {::hf/default (or needle "")}])})
      ;[sub-locations {::hf/options (all-locations [:needle {::hf/default (or needle "")}])}]
      ;^{::hf/options (all-locations ^{::hf/default (or needle "")} needle)}
      ;(sub-locations %)

      {^{::hf/options (subs-picklist needle)}
       'school/_subs                                        ; potential collision with a function var, and altered datomic syntax
       ['school/id
        'school/name]}

      #link sub-display                                     ; blue text

      #link ^{::hf/render (dom/a {:html/href *route*} (str "click here to see " (p/server (suber-name %)) " who is " gender))
              ::hf/label  (str "click here to see " (p/server (suber-name %)))}
      (sub-display %)

      #action
          (school-requested-block
            {'sub [sub-name]}
            {^{::hf/options      (schools .)
               ::hf/option-label :school/name}
             'school
             ['school/id
              'school/name]}
            {^{::hf/options      (block-reasons .)
               ::hf/option-label :db/ident}
             'block-reason
             ['db/ident]})]})

  (defn school-requested-block [& args]
    [:db/add]
    #_(doto (dom/field ...) (set! *tx* [:db/add])))

  )