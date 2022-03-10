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

  ; Photon DAG
  (p/defn system [{:keys [host port] :as config}]
    (let [database  (Database host port)
          scheduler (Scheduler)]
      (Example-component database scheduler config)))


  )
