(popover {(props (Deactivate-district. district .) {:render ...}) []})
(props (Deactivate-district. district .) {:render Popover})


(props (deactivate-district-q district reason) {::hf/popover true
                                                ::hf/popover-anchor "deactivate district"
                                                ::hf/render list-of-forms-renderer
                                                ::hf/tx (deactivate-district-tx district reason)})
(hfql
  [:district/id :district/name (DeactivateDistrictPopoverAnchor. district) :district/billing-email :district/payment-type])




(props (deactivate-district-tx district .) {::hf/popover "deactivate district"})



(props (deactivate-district-tx district .) {::hf/render (p/fn [{::hf/keys [Value] :as ctx}]
                                                          (Popover.
                                                            (p/fn [_] "deactivate district")
                                                            Value))})


(p/defn DeactivateDistrictBody [district]
  (dom/h1 "Deactivate District")
  (dom/div "Deactivating a district will deactivate all schools in the district, meaning:")
  (dom/ul
    (dom/li "unlink " (dom/strong "all subs") " and " (dom/strong "all admins") " from all the
    schools in the district,")
    (dom/li "tag all schools with \":school.map/hide\",")
    (dom/li "tag all schools with the provided reason."))

  (dom/label "district")
  (dom/Input. (p/server (pr-str (:district/id (d/entity rosie/db district)))) #_{:disabled true})


  (let [reason (p/with-cycle [reason :category/terminated-contract]
                 (dom/label "reason")
                 (rosie/read-edn-str (dom/Input. (pr-str reason))))]

    (when (valid? reason district)

      (let [xs (deactivate-district-query district reason)]

        (let [tx (hf/Render xs)]

          (into-tx tx `[(deactivate-district-tx ~district ~reason)])


          )))


    (when (and (some? district) (keyword? reason)) ; todo validate spec
      `[(deactivate-district-tx ~district ~reason)])))