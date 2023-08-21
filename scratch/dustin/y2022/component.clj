(comment

  ; System DAG specified as Component
  (defn example-system [{:keys [host port] :as config}]
    (component/system-map
      :db (new-database host port)
      :scheduler (new-scheduler)
      :app (component/using
             (example-component config)
             {:database  :db
              :scheduler :scheduler})))

  ; Electric DAG w/ reactive bindings
  (e/defn Example-system [{:keys [host port] :as config}]
    (binding [db (Database. host port)
              scheduler (Scheduler.)
              app (Example-component. config)]
        ; return events if you want
      ))
  
  )
