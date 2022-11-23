(ns hfql_examples
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.hfql :refer [hfql]]))

(comment


  [{(sub-requests sub
                  since
                  {{status {:options (sub-req-status-picklist)}} [:db/ident]}
                  {{school {:options (school-picklist .)}} [:school/name]})
    [{:sub-req/school
      [:school/name]}
     :sub-req/status
     :sub-req.index/start
     :sub-req.index/end]}]

  )

(comment

  {(orders .)
   [:db/id
    :order/email
    :order/tags
    {:order/gender
     [:db/ident]}
    {(props :order/shirt-size {::hf/option-label (name (:db/ident hf/v))
                               ::hf/options (shirt-sizes gender .)})
     [:db/ident]}]}



  {(orders .)
   [:db/id
    :order/email
    :order/tags
    {:order/gender $ {::hf/option-label (name (:db/ident hf/v))
                      ::hf/options (genders)}
     [:db/ident]}
    {:order/shirt-size $ {::hf/option-label (name (:db/ident hf/v))
                          ::hf/options (shirt-sizes gender .)}
     [:db/ident]}]}



  {(orders .)
   [:db/id
    :order/email
    :order/tags
    {(props :order/gender {::hf/option-label (name (:db/ident hf/v))
                           ::hf/options (genders)})
     [:db/ident]}
    {(props :order/shirt-size {::hf/option-label (name (:db/ident hf/v))
                               ::hf/options (shirt-sizes gender .)})
     [:db/ident]}]}

  )

(comment

  (hfql
    [:db/id
     swinged.rosie.account/suber-email
     :sub/phone
     :sub/tags
     sub-locations
     {sub-metrics
      [:sub.metrics/feedback-score
       :sub.metrics/reliability-score]}
     :sub/phone-confirmed
     suber-name
     onboard-date
     :sub/photo
     :sub/id
     days-worked
     :sub.index/cancel-rate
     :sub/about
     {:esub/region
      [:region/id]}
     {:sub/attrs
      [:sub-attr/keyword :sub-attr/name :sub-attr/expiry :sub-attr/status
       :sub-attr/document-ref :sub-attr/document-link :sub-attr/execution-date]}




     {:sub/experience
      [:db/id :sub-exp/organization :sub-exp/role :sub-exp/start :sub-exp/end
       :sub-exp/type :sub-exp/tags]}



     {:sub/priority
      [:sub-priority/id]}])

  )