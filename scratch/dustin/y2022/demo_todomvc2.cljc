(ns dustin.y2022.demo-todomvc2
  (:require clojure.edn
            [datascript.core :as d]
            [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.demo-todomvc2)))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil))       ; server
(p/def db)                                                  ; server
(p/def next-id!)                                            ; client
(p/def basis-t)                                             ; client
(def !state #?(:cljs (atom {::filter :all})))               ; client

(defn query-todos [db filter]
  {:pre [filter]}
  (case filter
    :active (d/q '[:find [?e ...] :where [?e :task/status :active]] db)
    :done (d/q '[:find [?e ...] :where [?e :task/status :done]] db)
    :all (d/q '[:find [?e ...] :where [?e :task/status]] db)))

(defn todo-count [db filter]
  {:pre  [filter]
   :post [(number? %)]}
  (-> (case filter
        :active (d/q '[:find (count ?e) . :where [?e :task/status :active]] db)
        :done (d/q '[:find (count ?e) . :where [?e :task/status :done]] db)
        :all (d/q '[:find (count ?e) . :where [?e :task/status]] db))
      ; datascript can return nil wtf
      (or 0)))

(p/defn Filter-control [state target label]
  (dom/a {:class (if (= state target) "selected")}
         (dom/text label)
         (when (dom/events "click")
           ; emit next filter value
           target)))

;(defmacro collect [& body] `(m/ap (m/amb=)))

(defn collect
  ([a b] (m/ap (m/amb= (m/?> a) (m/?> b))))
  ([a b c] (m/ap (m/amb= (m/?> a) (m/?> b) (m/?> c)))))

(p/defn TodoStats [state]                                   ; returns tx for cleared todos
  (let [active (p/server (todo-count db :active))
        done (p/server (todo-count db :done))
        #_#_[active done] (mapv (partial todo-count db) [:active :done])]
    (println `active active)
    (println `done done)
    (dom/div
      (dom/span
        {:class "todo-count"}
        (dom/strong (dom/text active))
        (dom/span (dom/text (str " " (case active 1 "item" "items") " left"))))

      (concat                                               ; collect tx
        (when-let [filter' (dom/ul
                             {:class "filters"}
                             (->>
                               (collect
                                 (p/fn [] (dom/li (Filter-control. (::filter state) :all "All")))
                                 (p/fn [] (dom/li (Filter-control. (::filter state) :active "Active")))
                                 (p/fn [] (dom/li (Filter-control. (::filter state) :done "Completed"))))
                               (m/relieve {})
                               new))]
          ; careful to dedupe state, this can infinite loop otherwise
          (swap! !state assoc ::filter filter') nil)        ; don't accidentally emit a tx

        (when (pos? done)
          (dom/button
            {:class "clear-completed"}
            (dom/text (str "Clear completed " done))
            (when (dom/events "click")                 ; bug - stays up too long if more todos complete
              (p/for [id (p/server (query-todos db :done))]
                {:db/id id :task/status :done}))))))))

(p/defn TodoItem [id]
  (p/server
    (let [x #_{:keys [task/status
                      task/description]} (d/entity db id)
          status (:task/status x)
          description (:task/description x)]
      (p/client
        (dom/li
          {:class (case status :done "completed" :active "editing")}
          (concat
            (ui/checkbox {::ui/value       (#{:done} status)
                          ::ui/input-event (p/fn [e]
                                             [{:db/id id :task/status (case (-> e :target :checked)
                                                                        false :active
                                                                        true :done)}]
                                             #_(reset! !running ))})
            (dom/span (dom/text (str description)))))))))

(p/defn TodoList [state]
  (p/client
    (let [active (p/server (todo-count db :active))]
      (dom/section
        {:class "main"}
        (->>
          (collect
            (p/fn []
              (ui/checkbox {:class "toggle-all"
                            ::ui/value       (zero? active)
                            ::ui/input-event (p/fn [e]
                                               ; Toggle all to done, unless everything is already done, in which case toggle all to active.
                                               (let [status' (if (pos? active) :done :active)]
                                                 (p/for [id (p/server (query-todos db :active))]
                                                   {:db/id id :task/status status'})))}))
            (p/fn []
              (dom/ul
                {:class "todo-list"}
                (apply concat                               ; merge txes
                       (p/for [id (p/server (query-todos db (::filter state)))]
                                (TodoItem. id)))))
            ) (m/relieve {}) new)))))

(p/defn CreateTodo []
  (ui/input
    {:placeholder        "What needs to be done?"
     ::ui/keychords      #{"enter"}
     ::ui/keychord-event (p/fn [e]
                           (let [description (dom/oget dom/node :value)]
                             (dom/oset! dom/node :value "")
                             [{:task/description description
                               :task/status      :active
                               :db/id            (str ::tempid "-" (next-id!))}]))}))

(p/defn TodoMVC "returns transactions" [state]
  (dom/div
    (concat
      (dom/section
        {:class "todoapp"}
        (dom/header
          {:class "header"}
          (dom/h1 (dom/text "TodoMVC"))
          (CreateTodo.)))

      (when (p/server (pos? (todo-count db :all)))
        (TodoList. state)
        (dom/footer
          {:class "footer"}
          #_(dom/text "hi")                                 ; crashes if empty footer
          (TodoStats. state)))

      (dom/footer
        {:class "info"}
        (dom/p (dom/text "Double-click to edit a todo")))

      (dom/div
        (dom/dl
          (dom/dt (dom/text "count :all")) (dom/dd (dom/text (pr-str (p/server (todo-count db :all)))))
          (dom/dt (dom/text "query :all")) (dom/dd (dom/text (pr-str (p/server (query-todos db :all)))))
          (dom/dt (dom/text "state")) (dom/dd (dom/text (pr-str state))))))))

(defn transact [tx] #?(:clj (do (prn `transact tx) (d/transact! !conn tx) nil)))

(p/defn App []
  (p/client
    (let [!t (atom 0)
          state (->> (m/watch !state) (m/eduction (dedupe)) (m/relieve {}) new)] ; prevent infinite loop on setting same filter
      (binding [basis-t (p/watch !t)                        ; basis-t is used to ack completed transact to controls
                next-id! (partial swap! (atom 0) inc)]
        (prn `basis-t basis-t)
        (p/server
          (binding [db (p/watch !conn)]
            (p/client
              (if-some [tx (seq (doto (TodoMVC. state) (println `tx)))]
                (swap! !t + (do (p/server (transact tx)) 1))         ; auto-transact
                (prn :idle)))))))))

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
