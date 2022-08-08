(ns user.demo-5-todomvc
  "Requires -Xss2m to compile. default 1m JVM ThreadStackSize is exceeded by photon compiler due to large macroexpansion
  resulting in false StackOverflowError during analysis."
  (:require [datascript.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.photon-dom :as dom])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros user.demo-5-todomvc)))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil))       ; server
(p/def db)                                                  ; server
(def !state #?(:cljs (atom {::filter :all                   ; client
                            ::editing nil})))

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
  (ui/element dom/a {::dom/class (when (= state target) "selected")
                     ::ui/click-event (p/fn [_] [::set-filter target])}
    (dom/text label)))

(defn transact! "prevent remote errors (attempt to serialize and move d/transact return value)"
  [!conn tx]
  #?(:clj (do (d/transact! !conn tx) nil)
     :cljs (assert false "transact from wrong peer (called on: client)")))

(p/defn TodoStats [state]
  (let [active (p/server (todo-count db :active))
        done   (p/server (todo-count db :done))]
    (dom/div
      (dom/span {:id "todo-count"}
        (dom/strong (dom/text active))
        (dom/span (dom/text (str " " (case active 1 "item" "items") " left"))))

      (dom/ul {:id "filters"}
        (dom/li (Filter-control. (::filter state) :all "All"))
        (dom/li (Filter-control. (::filter state) :active "Active"))
        (dom/li (Filter-control. (::filter state) :done "Completed")))

      (when (pos? done)
        (ui/button {::dom/id         "clear-completed"
                    ::ui/click-event (p/fn [_] [::clear :done])}
          (dom/text (str "Clear completed " done)))))))

(defn focus! [node] (.focus node))

(p/defn TodoItem [state id]
  (p/server
    (let [x           #_ {:keys [:task/status :task/description]} (d/entity db id) ; Unable to resolve - clojure.core/--destructure-map
          status      (:task/status x)
          description (:task/description x)]
      (p/client
        (dom/li
          {:class [(when (= :done status) "completed")
                   (when (= id (::editing state)) "editing")]}
          (dom/div {:class "view"}
            (ui/checkbox {::dom/class      "toggle"
                          ::ui/value       (= :done status)
                          ::ui/input-event (p/fn [e] (case (-> e :target :checked)
                                                       true  [::set-done id]
                                                       false [::set-active id]
                                                       nil))})
            (ui/element dom/label {::ui/dblclick-event (p/fn [_] [::editing id])}
              (dom/text (str description))))
          (when (= id (::editing state))
            (ui/input {::dom/class         "edit"
                       ::dom/autofocus     true
                       ::ui/value          description
                       ::ui/keychords      #{"enter" "esc"}
                       ::ui/keychord-event (p/fn [e] (case (:identifier e)
                                                       "esc"   [::done-editing nil]
                                                       "enter" [[::set-description [id (-> e :target :value)]]
                                                                [::done-editing nil]]))}
              (focus! dom/node)))
          (ui/button {::dom/class      "destroy"
                      ::ui/click-event (p/fn [_] [::destroy id])}))))))

(p/defn TodoList [state]
  (p/client
    (dom/div
      (dom/section {:id "main"}
        (let [active                     (p/server (todo-count db :active))
              all                        (p/server (todo-count db :all))
              done                       (p/server (todo-count db :done))]
          (ui/checkbox {::dom/id         "toggle-all"
                        ::ui/value       (cond (= all done)   true
                                               (= all active) false
                                               :else          nil)
                        ::ui/input-event (p/fn [_] (if (= all done) [::toggle-all :active] [::toggle-all :done]))}))
        (dom/label {:for "toggle-all"} (dom/text "Mark all as complete"))
        (dom/ul {:id "todo-list"}
          (p/for [id (p/server (sort (query-todos db (::filter state))))]
            (TodoItem. state id)))))))

(p/defn CreateTodo []
  (ui/input
    {::dom/id            "new-todo"
     ::dom/placeholder   "What needs to be done?"
     ::ui/keychords      #{"enter"}
     ::ui/keychord-event (p/fn [e] (let [description (:value dom/node)]
                                     (dom/oset! dom/node :value "")
                                     [::create-todo description]))}))

(p/defn TodoMVC "returns transactions" [state]
  (p/client
    (dom/div
      (dom/section
        {:id "todoapp"}
        (dom/header
          {:id "header"}
          (dom/h1 (dom/text "TodoMVC"))
          (CreateTodo.))

        (when (p/server (pos? (todo-count db :all)))
          (TodoList. state))

        (dom/footer
          {:id "footer"}
          (TodoStats. state)))

      (dom/footer
        {:id "info"}
        (dom/p (dom/text "Double-click to edit a todo"))))))

(defn commands->tx
  "Map `commands` to a datomic tx."
  [db commands]
  (prn "COMMANDS" commands)
  (->> commands
    (mapcat (fn [[tag value]] (case tag
                                ::create-todo     [{:task/description value, :task/status :active}]
                                ::set-done        [{:db/id value, :task/status :done}]
                                ::set-active      [{:db/id value, :task/status :active}]
                                ::clear           (map (fn [id] [:db/retractEntity id]) (query-todos db :done))
                                ::set-description (let [[id description] value]
                                                    [{:db/id id, :task/description description}])
                                ;; Toggle all to done, unless everything is already done, in which case toggle all to active.
                                ::toggle-all      (when (#{:done :active} value)
                                                    (let [ids (query-todos db (if (= :done value) :active :done))]
                                                      (map (fn [id] {:db/id id, :task/status value}) ids)))
                                ::destroy         [[:db/retractEntity value]])))
    (filter some?)
    (vec)))

(defn interpret-commands! [commands]
  (prn "INTERPRET" commands)
  (->> commands
    (map (fn [[tag value :as event]]
           (case tag
             ::set-filter   (do (swap! !state assoc ::filter value) nil)
             ::editing      (do (swap! !state assoc ::editing value) nil)
             ::done-editing (do (swap! !state assoc ::editing nil) nil)
             event)))
    (filter some?)))

(p/defn App []
  (p/client
    (let [state (p/watch !state)]
      (p/server
        (binding [db (p/watch !conn)]
          (p/client
            (dom/link {:rel :stylesheet, :href "todomvc.css"})
            (let [commands (p/deduping (TodoMVC. state))]  ; FIXME deduping impacted by Pending
              (dom/h1 (dom/text "Diagnostics"))
              (dom/div
                (dom/dl
                  (dom/dt (dom/text "count :all")) (dom/dd (dom/text (pr-str (p/server (todo-count db :all)))))
                  (dom/dt (dom/text "query :all")) (dom/dd (dom/text (pr-str (p/server (query-todos db :all)))))
                  (dom/dt (dom/text "state")) (dom/dd (dom/text (pr-str state)))))
              (let [server-commands (interpret-commands! commands)]
                (p/server
                  (prn "COMMAND" server-commands)
                  (let [tx (p/deduping (commands->tx db server-commands))]
                    (when (seq tx)
                      (prn "TX" tx)
                      (transact! !conn tx))))))))))))

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
