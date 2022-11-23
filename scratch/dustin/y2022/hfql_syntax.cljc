(ns .)

[:db/id
 swinged.rosie.account/suber-email
 :sub/phone
 :sub/tags
 {sub-locations {:options (sub-locations sub)}}
 {sub-metrics [:sub.metrics/feedback-score
               :sub.metrics/reliability-score]}
 :sub/phone-confirmed
 suber-name
 onboard-date
 :sub/photo
 :sub/id
 days-worked
 :sub.index/cancel-rate
 :sub/about
 {{:esub/region {:options (regions .)}}
  [:region/id]}

 {:sub/attrs
  [:sub-attr/keyword :sub-attr/name :sub-attr/expiry :sub-attr/status :sub-attr/document-ref
   :sub-attr/document-link :sub-attr/execution-date #_:db/id]}
 {:sub/experience
  [:db/id :sub-exp/organization :sub-exp/role :sub-exp/start :sub-exp/end :sub-exp/type :sub-exp/tags]}
 {:sub/priority [:sub-priority/id]}]


