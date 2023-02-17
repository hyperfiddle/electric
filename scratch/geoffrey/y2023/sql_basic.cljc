(ns geoffrey.2023.sql-basic
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom :as dom]))


;; Principles:
;; 1. The database is the source of thruth
;; 2. Database state changes through time
;; 3. Query results should be diffed as close as possible to the database
;; 4. Blocking (io bound) operations should not block the program from running

;; I'm writing in verbose style for clarity of communication.

(e/def pg-conn)
(e/def !db-time)

(defn list-pockemon-query [_db-time conn]
  ;; We ignore db-time parameter and (re)run the query
  (sql/list-pokemon conn))

(defn insert-pockemon-command! [!db-time conn data]
  (try
    (let [returned-value (sql/insert-pokemon conn data)]
      (swap! !db-time inc) ; if INSERT succeeds bump `!db-time` to trigger a query rerun.
      returned-value)
    (catch SQLException e
      ;; handle exception if needed
      (throw e)
      )))

(e/defn AddPokemon []
  (e/client
    (InputSubmit. (e/fn [v]
                    (e/server
                      (do (e/wrap (insert-pockemon-command! !db-time pg-conn {:id (random-uuid) :display_name v})) ; blocking operation⁴
                          nil))))))

(e/defn App []
  (try
    (e/server
      (binding [!db-time (atom 0)
                pg-conn  (sql/create-connection)]
        (e/client
          (dom/h1 (dom/text "Pokemons"))
          (AddPokemon.)
          (dom/div
            (e/server
              (let [db-time      (e/watch !db-time) ; inserts will bump (inc) this value
                    query-result (e/wrap ; Querying is a blocking operation⁴. We
                                         ; `wrap` it into a task. It will run on
                                         ; a IO optimized thread pool
                                   (list-pokemon-query db-time pg-conn)) ; will rerun every time `db-time` changes
                    ]
                ;; We diff on the server, immediately after we queried³. So
                ;; individual rows flow through the program. Not the whole query
                ;; result.
                (e/for-by :pokemon/id [{:keys [pokemon/display_name]} query-result]
                  (e/client
                    (dom/p (dom/text display_name))))))))))
    (catch hyperfiddle.electric.Pending e
      ;; A Pending exception is thrown when a task (`wrap`) is running, or when
      ;; a the client is parked (awaiting) for a first value from the server.
      ;; This is Electric try/catch! Not regular try/catch. It knows how to
      ;; resume the `try` branch exactly where it was before the exception was
      ;; thrown. So the `try` content will remain visible on the page while
      ;; Pending is handled. The content of the `try` will just be "paused" for
      ;; the duration of the Pending.
      (prn "Loading PokeDex…")
      )))
