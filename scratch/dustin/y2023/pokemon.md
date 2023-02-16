# PostgreSQL hello world — Electric Clojure


### How do I integrate a SQL database backend?

Easy:
* make an ordinary Clojure function `query-pokemon-list` for the query
* The query is blocking, and Electric Clojure is async, so use `e/wrap` to move it to a thread pool. (Don't block the reactor!)
* `e/wrap` throws `Pending` until the query finishes, and then the exception "goes away" (Electric exceptions are reactive!)

```clojure
(e/def pg-conn)

(defn query-pokemon-list [pg-conn] ...) ; Careful: JDBC is blocking

(e/defn App []
  (try
    (e/server
      (binding [pg-conn (sql/create-connection)]
        
        (let [pokemons (e/wrap ; move to thread pool
                         (query-pokemon-list pg-conn))] ; blocking
          
          (e/client
            (dom/h1 (dom/text "Pokemons"))
            (dom/div
              
              (e/server
                ; e/for-by is differential. It does diffing to stabilize the children on :pokemon/id, like 
                ; a React.js "key". We want to do the diffing on the server, so that only the DELTAS are 
                ; sent to the client.
                (e/for-by :pokemon/id [{:keys [pokemon/display_name]} pokemons]
                  (e/client
                    ; only runs when display_name has changed for this :pokemon/id
                    (dom/p (dom/text display_name))))))))))
    (catch Pending _
      (dom/props {:class "loading" :style {:background-color "yellow"}}))))
```

### How do I refresh the queries when a new Pokemon is added to the database?

Problem: batch/streaming impedance mismatch
* PostgreSQL is a batch database with request/response data paradigm
* Electric Clojure is a reactive programming language, which is a streaming† data paradigm
  * †Electric is about Signals not Streams, but the difference is not relevant here

Solution:
* We need a way to know when to re-run the queries
* What are some options?
  * re-run on navigate, like older apps
  * use PG LISTEN/NOTIFY -- https://yogthos.net/posts/2016-11-05-LuminusPostgresNotifications.html
  * use PG Write Ahead Log
  * use a streaming SQL db like Materialized
  * use a streaming event source for the realtime views of our app and timers or navigate for slower views

That all sounds hard so for this tutorial let's just track the dirty state ourselves.

```clojure
(e/def pg-conn)
(e/def !dirty) ; hack
(e/def dirty)

(e/defn AddPokemon []
  (e/client
    (InputSubmit. (e/fn [v]
                    (e/server
                      (try
                        (let [x (e/wrap (sql/insert-pokemon pg-conn {:id (random-uuid) :display_name v}))]
                          (swap! !dirty inc) ; success
                          x)
                        (catch SQLException e
                          ; handle it, or alternatively let it propagate upward
                          (e/client (dom/props {:class "error" :style {:background-color "red"}})))))))))

(e/defn App []
  (try
    (e/server
      (binding [pg-conn (sql/create-connection)
                !dirty (atom 0) ; make available anywhere that pg-conn is available
                dirty (e/watch !dirty)] ; the reactive value has different equality semantics than 
                ; the reference; we must treat them separately to avoid superfluous reactions 

        (let [pokemons (e/wrap (sql/list-pokemon pg-conn dirty))] ; reruns when dirty changes
          (e/client
            (dom/h1 (dom/text "Pokemons"))
            (AddPokemon.)
            (dom/div
              (e/server
                (e/for-by :pokemon/id [{:keys [pokemon/display_name]} pokemons]
                  (e/client
                    (dom/p (dom/text display_name))))))))))
    (catch Pending _
      (dom/props {:class "loading" :style {:background-color "yellow"}}))))
```

Takeaways:
1. queries are just Clojure functions
2. Electric is async; be careful not to block the reactor
3. use `e/wrap` to move blocking fns to a theadpool
4. `e/for-by` is differential, make sure to do the diffing on the server
5. the query will re-run when a parameter changes (as per `=`)
6. Data layer is still your problem, but Electric gives you the power you need
