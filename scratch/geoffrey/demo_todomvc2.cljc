(ns geoffrey.demo-todomvc2
  (:require [datascript.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.photon-dom :as dom])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros geoffrey.demo-todomvc2)))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil))       ; server
(p/def db)                                                  ; server
(def !state #?(:cljs (atom {::filter :all})))               ; client

(defn query-todos [db filter]
  {:pre [filter]}
  (case filter
    :active (d/q '[:find [?e ...] :where [?e :task/status :active]] db)
    :done   (d/q '[:find [?e ...] :where [?e :task/status :done]] db)
    :all    (d/q '[:find [?e ...] :where [?e :task/status]] db)))

(defn todo-count [db filter]
  {:pre  [filter]
   :post [(number? %)]}
  (-> (case filter
        :active (d/q '[:find (count ?e) . :where [?e :task/status :active]] db)
        :done   (d/q '[:find (count ?e) . :where [?e :task/status :done]] db)
        :all    (d/q '[:find (count ?e) . :where [?e :task/status]] db))
      ; datascript can return nil wtf
      (or 0)))

(p/defn Filter-control [state target label]
  (ui/button {::dom/class (if (= state target) "selected")
              ::ui/click-event (p/fn [_]
                                 (swap! !state assoc ::filter (p/deduping target)))}
    (dom/text label)))

(defn transact! "prevent remote errors (attempt to serialize and move d/transact return value)"
  [!conn tx]
  #?(:clj (do (d/transact! !conn tx) nil)
     :cljs (assert false "transact from wrong peer (called on: client)")))

(p/defn TodoStats [state]
  (let [active (p/server (todo-count db :active))
        done   (p/server (todo-count db :done))]
    (dom/div
      (dom/span
        {:class "todo-count"}
        (dom/strong (dom/text active))
        (dom/span (dom/text (str " " (case active 1 "item" "items") " left"))))

      (dom/ul
        {:class "filters"}
        (dom/li (Filter-control. (::filter state) :all "All"))
        (dom/li (Filter-control. (::filter state) :active "Active"))
        (dom/li (Filter-control. (::filter state) :done "Completed")))

      (when (pos? done)
        (ui/button
          {::dom/class "clear-completed"
           ::ui/click-event (p/fn [_] ; bug - stays up too long if more todos complete
                              (let [tx (p/for [id (p/server (query-todos db :done))]
                                         [:db/retractEntity id])]
                                (p/server (transact! !conn tx))))}
          (dom/text (str "Clear completed " done)))))))

(p/defn TodoItem [id]
  (p/server
    (let [x           #_ {:keys [:task/status :task/description]} (d/entity db id) ; Unable to resolve - clojure.core/--destructure-map
          status      (:task/status x)
          description (:task/description x)]
      (p/client
        (dom/li
          {:class (case status :done "completed" :active "editing")}
          (ui/checkbox {::ui/value       (#{:done} status)
                        ::ui/input-event (p/fn [e]
                                           (let [tx [{:db/id id :task/status (case (-> e :target :checked)
                                                                               false :active true :done)}]]
                                             (p/server (transact! !conn tx))))})
          (dom/span (dom/text (str description))))))))

(p/defn TodoList [state]
  (p/client
    (dom/section
      {:class "main"}
      (let [active (p/server (todo-count db :active))]
        (ui/checkbox {::dom/class      "toggle-all"
                      ::ui/value       (zero? active)
                      ::ui/input-event (p/fn [e]
                                        ; Toggle all to done, unless everything is already done, in which case toggle all to active.
                                         (let [status' (if (pos? active) :done :active)
                                               tx      (p/for [id (p/server (query-todos db :active))]
                                                         {:db/id id :task/status status'})]
                                           (p/server (transact! !conn tx))))}))
      (dom/ul
        {:class "todo-list"}
        (p/for [id (p/server (query-todos db (::filter state)))]
          (TodoItem. id))))))

(p/defn CreateTodo []
  (ui/input
    {::dom/placeholder   "What needs to be done?"
     ::ui/keychords      #{"enter"}
     ::ui/keychord-event (p/fn [e]
                           (let [description (dom/oget dom/node :value)]
                             (dom/oset! dom/node :value "")
                             (p/server (transact! !conn [{:task/description description
                                                          :task/status      :active}]))))}))

(p/defn TodoMVC "returns transactions" [state]
  (p/client
    (dom/div
      (dom/section
        {:class "todoapp"}
        (dom/header
          {:class "header"}
          (dom/h1 (dom/text "TodoMVC"))
          (CreateTodo.)))

      (when (p/server (pos? (todo-count db :all)))
        (TodoList. state))

      (dom/footer
        {:class "footer"}
        (TodoStats. state))

      (dom/footer
        {:class "info"}
        (dom/p (dom/text "Double-click to edit a todo"))))))

(p/defn App []
  (p/client
    (let [state (p/watch !state)]
      (p/server
        (binding [db (p/watch !conn)]
          (p/client
            (TodoMVC. state)
            (dom/h1 (dom/text "Diagnostics"))
            (dom/div
              (dom/dl
                (dom/dt (dom/text "count :all")) (dom/dd (dom/text (pr-str (p/server (todo-count db :all)))))
                (dom/dt (dom/text "query :all")) (dom/dd (dom/text (pr-str (p/server (query-todos db :all)))))
                (dom/dt (dom/text "state")) (dom/dd (dom/text (pr-str state)))))))))))

(def main #?(:cljs (p/boot (try (binding [dom/node (dom/by-id "root")] (App.)) (catch Pending _)))))

(comment
  (user/browser-main! `main)
  (todo-count @!conn :all)
  (todo-count @!conn :active)
  (todo-count @!conn :done)
  (query-todos @!conn :all)
  (query-todos @!conn :active)
  (query-todos @!conn :done)
  (d/q '[:find (count ?e) . :where [?e :task/status]] @!conn)
  )
