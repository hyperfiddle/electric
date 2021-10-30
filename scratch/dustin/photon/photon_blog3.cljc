(p/defn query-persons [db needle]
        (server
          (datomic.api/q '[:find [?e ...] :in $ ?needle :where
                           [?e :person/email ?email]
                           [(clojure.string/includes? ?email ?needle)]]
                         db (or needle ""))))

(p/defn render-persons [db needle]
        (client
          (dom/div
            (dom/h1 "submissions")
            (dom/table
              (server
                (photon/for [e (query-persons db needle)]
                  (let [{:keys [:db/id
                                :person/email
                                :person/gender
                                :person/shirt-size]} (datomic.api/entity db e)]
                    (client
                      (dom/tr
                        (dom/td (str id))
                        (dom/td email)
                        (dom/td (pr-str gender))
                        (dom/td (pr-str shirt-size)))))))))))


(p/defn typeahead-select [v options]
        (client
          (dom/div
            (let [needle (dom/input "")]
              (dom/select
                (p/for [e (p/$ options needle)]
                       (dom/option (dom/span (server (name o))))))))))

(p/defn example-form [db e]
        (client
          (dom/form
            (dom/field
              (typeahead-select
                e
                (p/fn [needle]
                      (server
                        (let [gender (:dustingetz/gender (datomic.api/entity db e))
                              es (datomic.api/q '[:in $ ?gender :find [?e ...] :where
                                                  [?e :dustingetz/type :dustingetz/shirt-size]
                                                  [?e :dustingetz/gender ?gender]]
                                                db gender (or needle ""))]
                          (for [e es]
                            (client
                              (dom/option (server (:db/ident (datomic.api/entity db e))))))))))))))