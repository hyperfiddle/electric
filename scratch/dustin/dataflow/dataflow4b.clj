(ns dustin.dataflow4b)

(comment

  ; server
  ; server constructs program

  (def crud-app '(let [[needle] (<- >route)]
                   (effect-async (partial swap! effects conj)
                     (vector (render-table (<- (map-task submission-query (extend needle))))
                       (if (<- >open)
                         (render-table (<- (map-task submission-query (extend "alice"))))
                         ::nothing)))))

  (def dag (analyze-clj crud-app))

  (send-dag-to-client! dag)

  (def server-process (interpret-dag {`render-table nil
                                      `submission-query (m/via m/cpu (datomic.api/q ...))}
                        dag))


  ; client
  (def dag (recv-dag-from-server! *ws*))

  (def ui-process (interpret-dag {`render-table (from-trace)
                                  `submission-query (m/via m/cpu ...)

                                  }
                    dag))
  (def cancel (ui-process prn prn))



  ; sends program to client
  ; client runs program

  ; client

  )