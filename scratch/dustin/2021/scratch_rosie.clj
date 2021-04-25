(comment

  (M [(M (M x))
      (M (M x))])

  ; 1. applicative or monad
  ; 2. route and entrypoint

  (def route `(sub-requests [:sub/id ...] nil nil nil))
  (def route `(sub-display [:sub/id ...]))

  (def Rosie
    '{(sub-display sub/id) [:sub/id
                            {(suber-name-kv sub/id) [:db/id]}
                            {(rosie-entity-history sub/id) (fn [%]
                                                             [*
                                                              :db/id
                                                              (match (rosie-type %)
                                                                ::admin [:admin/name
                                                                         :admin/id
                                                                         {(admin-display admin/id) ...}]
                                                                ::sub [:sub/name
                                                                       :sub/id
                                                                       (sub-display sub/id)]
                                                                _ nil)])}
                            {(all-locations sub/id) [:db/ident]}
                            {(sub-requests sub/id since ?status ?school) [:sub-req.index/start
                                                                          :sub-req.index/end
                                                                          :school/name
                                                                          :sub-req/id
                                                                          {(sub-request sub-req/id) [:sub-req/grade
                                                                                                     :sub-req/subject
                                                                                                     :sub-req/memo
                                                                                                     {:sub-req/status [:db/ident]}
                                                                                                     :sub/id
                                                                                                     {(sub-display sub/id) ...}]}]}]})

  (def route `(sub-requests sub))
  (def route `{(sub-display [:sub/id ...]) [...]})


  (def Rosie2-fiddle-registry
    '{(sub-display sub/id)          [:sub/id
                                     (suber-name-kv sub/id)
                                     (rosie-entity-history sub/id)
                                     (all-locations sub/id)
                                     (sub-requests sub/id since ?status ?school)]
      (sub-requests sub/id)         [:sub-req.index/start
                                     :sub-req.index/end
                                     :school/name
                                     :sub-req/id
                                     (sub-request sub-req/id)]
      (sub-request sub-req/id)      [:sub-req/grade
                                     :sub-req/subject
                                     :sub-req/memo
                                     {:sub-req/status [:db/ident]}
                                     :sub/id
                                     (sub-display sub/id)]
      (suber-name-kv sub/id)        [:db/id]
      (all-locations sub/id)        [:db/ident]
      (rosie-entity-history sub/id) (fn [%]
                                      [*
                                       :db/id
                                       (match (rosie-type %)
                                         ::admin [:admin/name
                                                  :admin/id
                                                  {(admin-display admin/id) ...}]
                                         ::sub [:sub/name
                                                :sub/id
                                                (sub-display sub/id)]
                                         _ nil)])
      })

  ;(def hfql-result (M (M (M x))))

  (def route `(sub-requests sub))

  (def >route (input))
  (def >result (hf-pull >route))                            ; server and client shared

  (loop (join (recur)))

  (def >view
    (fmap
      (join (sequence (join >result)))
      (fn [v] [:pre (js/pprint-str v)])))

  (on >view reactRomRender)
  (put >route `(sub-requests sub))
  (put >route `{(sub-requests sub) [:sub/id
                                    {(sub-display sub/id) [...]}]})

  (defn sub-display [sub]
    (d/entity hf/*$* sub))

  (defn rosie-entity-history [e before attr]

    )

  (defn suber-name-kv []
    {::rosie/name (suber-name e)})

  (defn all-locations []
    (d/q '[:find [?tag ...]
           :in $ ?needle
           :where
           [_ :sub/tags ?tag]

           [(namespace ?tag) ?ns] [(= ?ns "location")]

           [(name ?tag) ?name] [(swinged.norby2/needle-match ?name ?needle)]]
      hf/*$* needle))

  )