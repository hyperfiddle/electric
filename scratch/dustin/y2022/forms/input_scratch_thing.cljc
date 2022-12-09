
(comment
  (p/defn App []
    (p/client
      (dom/div
        (dom/h1 "System Properties")
        (let  [!form (atom ["" ""]) [a b] (p/watch !form)]
          (p/server
            (let [system-props (sort-by key (system-properties a))
                  matched-count (count system-props)]
              (p/client
                (ui/input {::dom/type :search
                           ::ui/value a
                           ::dom/placeholder "java.home"
                           ::ui/input-event (p/fn [e] (reset! !form [(:value dom/node)
                                                                     b]))})
                (dom/hr)
                (when-some [b' (ui/input {::ui/value b
                                          ::dom/type :search
                                          ::dom/placeholder "java.home"})]
                  (reset! !form [a b']))

                (dom/table
                  (p/server
                    (p/for-by first [[k v] system-props]
                      (p/client
                        (dom/tr
                          (dom/td k)
                          (dom/td {:style {:white-space :nowrap}} v)))))))))))))

  (p/defn App []
    (p/client
      (dom/div
        (dom/h1 "System Properties")
        (let [search (p/with-cycle [search ["" nil ""]] ; bubbling
                       (p/client
                         (dom/div
                           "asdf"
                           (ui/input {::dom/type :search
                                      ::dom/placeholder "java.home"
                                      #_#_::ui/input-event (p/fn [e] (reset! !search (:value dom/node)))})
                           (dom/hr) ; bubbling
                           (ui/input {::dom/type :search
                                      ::dom/placeholder "java.home"
                                      #_#_::ui/input-event (p/fn [e] (reset! !search (:value dom/node)))}))))]
          (p/server
            (let [system-props (sort-by key (system-properties search))
                  matched-count (count system-props)]
              (dom/table
                (p/server
                  (p/for-by first [[k v] system-props]
                    (p/client
                      (dom/tr
                        (dom/td k)
                        (dom/td {:style {:white-space :nowrap}} v))))))))))))
  )

(comment
  (p/defn DemoInput []
    (def edward 694891374557546)
    (p/client

      (dom/h1 "controlled input - database backed (latency) - google display name test")
      (let [a (p/with-cycle [a (p/server (:google/display-name (d/entity rosie/db edward)))]
                (-> (dom/Input. (or a ""))))]
        (dom/pre (pr-str [[:db/add edward :google/display-name a]])))

      (dom/h1 "controlled input with stage")
      (p/server
        (p/with-cycle [stage []]
          (let [db (:db-after (d/with rosie/db (or stage [])))
                m (d/pull db [:google/display-name :db/id] edward)
                a (:google/display-name m #_(d/entity db edward))] ; d/entity broken equality
            (p/client
              (dom/div "stage is: " (pr-str stage))
              (dom/div "entity is: " (p/server (contrib.str/pprint-str m)))
              (dom/div "display-name is: " (p/server a))
              (let [a (dom/Input. (or a ""))]
                (when a [[:db/add edward :google/display-name a]])))))
        nil))))