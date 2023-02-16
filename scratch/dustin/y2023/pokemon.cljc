(ns dustin.y2023.pokemon
  #?(:cljs (:require-macros dustin.y2023.pokemon))
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))




;; Context:
;; We want to react to database changes
;; Some databases are reactive by design (Materialize.com)
;; Some have interesting features:
;; - Firebase can notify for changes
;; - PostgreSQL have LISTEN/NOTIFY (example: https://yogthos.net/posts/2016-11-05-LuminusPostgresNotifications.html)
;; - one can also watch the database Write Ahead Log (WAL)

;; Solution:
;; We want this example to stay simple.
;; We simulate a DB notification with a `!dirty` atom.
;; If `!dirty` has changed, we rerun the query.
;; Inserting to DB will update `!dirty`.

;; Principles:
;; 1. The database is the source of thruth
;; 2. Database state changes through time
;; 3. Query results should be diffed as close as possible to the database
;; 4. Blocking (io bound) operations should not block the Electric Clojure program

(e/def conn)      ; The database connection instance
(e/def !dirty)    ; Will hold an atom. Queries should rerun if `!dirty` changes.

(defn list-pockemon-query [_dirty conn] ; read from db
  ;; We ignore the `dirty` param and (re)run the query
  (sql/list-pokemon conn))

(defn insert-pockemon-command! [!dirty conn data] ; write to db
  )

(e/defn AddPokemon [] ; UI text input. Prompt for a name and save to db.
  (e/client
    (InputSubmit. (e/fn [v]
                    (e/server
                      (e/wrap ; insert-pockemon-command! can block on IO.
                        ; `wrap` ensures it will not block the whole Electric Clojure program⁴.
                        (insert-pockemon-command! !dirty conn {:id (random-uuid) :display_name v}))
                      nil)))))

(e/defn App []
  (try
    (e/server
      (binding [!dirty (atom 0)
                conn   (sql/create-connection)]
        (e/client
          (dom/h1 (dom/text "Pokemons"))
          (AddPokemon.)
          (dom/div
            (e/server
              (let [dirty        (e/watch !dirty) ; inserts will bump this value
                    query-result (e/wrap ; Querying is a blocking operation⁴. We
                                   ; `wrap` it so it doesn’t block the program.
                                   ; It will run on a IO optimized thread pool.
                                   (list-pokemon-query dirty conn)) ; will rerun every time `dirty` changes
                    ]
                ;; We diff on the server, immediately after we queried³. So
                ;; individual rows flow through the program. Not the whole query
                ;; result.
                (e/for-by :pokemon/id [{:keys [pokemon/display_name]} query-result]
                  (e/client
                    (dom/p (dom/text display_name))))))))))
    (catch Pending _
      ;; A Pending exception is thrown when a task (`wrap`) is running, or when
      ;; a the client is parked (awaiting) for a first value from the server.
      ;; This is Electric try/catch! Not regular try/catch. It knows how to
      ;; resume the `try` branch where it was before the exception was
      ;; thrown. The `try` content will remain visible on the page while
      ;; Pending is handled. The content of the `try` will be "paused" for
      ;; the duration of the Pending.
      (prn "Loading PokeDex…")
      )))