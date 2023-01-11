(ns user.demo-5-todomvc
  "Requires -Xss2m to compile. default 1m JVM ThreadStackSize is exceeded by photon compiler due to large macroexpansion
  resulting in false StackOverflowError during analysis."
  (:require
   #?(:clj [datascript.core :as d])
   contrib.str
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-dom2 :as dom2]
   [hyperfiddle.photon-ui4 :as ui4])
  #?(:cljs (:require-macros user.demo-5-todomvc)))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil))       ; server
(p/def db)                                                  ; server
(p/def transact!) ; server
(def !state #?(:cljs (atom {::filter :all                   ; client
                            ::editing nil
                            ::delay   0})))

#?(:clj
   (defn query-todos [db filter]
     {:pre [filter]}
     (case filter
       :active (d/q '[:find [?e ...] :where [?e :task/status :active]] db)
       :done   (d/q '[:find [?e ...] :where [?e :task/status :done]] db)
       :all    (d/q '[:find [?e ...] :where [?e :task/status]] db))))

#?(:clj
   (defn todo-count [db filter]
     {:pre  [filter]
      :post [(number? %)]}
     (-> (case filter
           :active (d/q '[:find (count ?e) . :where [?e :task/status :active]] db)
           :done   (d/q '[:find (count ?e) . :where [?e :task/status :done]] db)
           :all    (d/q '[:find (count ?e) . :where [?e :task/status]] db))
       (or 0)))) ; datascript can return nil wtf

(p/defn Filter-control [state target label]
  (dom2/a (dom2/props {:class (when (= state target) "selected")})
    (dom2/text label)
    (dom2/on "click" (p/fn [_] (swap! !state assoc ::filter target)))))


(p/defn TodoStats [state]
  (let [active (p/server (todo-count db :active))
        done   (p/server (todo-count db :done))]
    (dom/div
      (dom/span {:class "todo-count"}
        (dom/strong active)
        (dom/span " " (str (case active 1 "item" "items")) " left"))

      (dom/ul {:class "filters"}
        (dom/li (Filter-control. (::filter state) :all "All"))
        (dom/li (Filter-control. (::filter state) :active "Active"))
        (dom/li (Filter-control. (::filter state) :done "Completed")))

      (when (pos? done)
        (ui4/button (p/fn [] (p/server (when-some [ids (seq (query-todos db :done))]
                                          (transact! (mapv (fn [id] [:db/retractEntity id]) ids)) nil)))
          (dom2/props {:class ["clear-completed"]})
          (dom2/text "Clear completed " done))))))

(p/defn TodoItem [state id]
  (p/server
    (let [{:keys [:task/status :task/description]} (d/entity db id)]
      (p/client
        (dom/li
          {:class [(when (= :done status) "completed")
                   (when (= id (::editing state)) "editing")]}
          (dom/div {:class "view"}
            (ui4/checkbox (= :done status) (p/fn [v]
                                              (let [status (case v true :done, false :active, nil)]
                                                (p/server (transact! [{:db/id id, :task/status status}]) nil)))
              (dom2/props {:class ["toggle"]}))
            (dom2/label (dom2/text description)
              (dom2/on "dblclick" (p/fn [_] (swap! !state assoc ::editing id)))))
          (when (= id (::editing state))
            (dom2/span (dom2/props {:class ["input-load-mask"]})
              (dom2/on-pending (dom2/props {:aria-busy true})
                (dom2/input
                  (dom2/bind-value description )
                  (dom2/on "keydown"
                    (p/fn [e]
                      (case (.-key e)
                        "Enter" (when-some [description (contrib.str/blank->nil (.-target.value e))]
                                  (case (p/server (transact! [{:db/id id, :task/description description}]) nil)
                                    (swap! !state assoc ::editing nil)))
                        "Escape" (swap! !state assoc ::editing nil)
                        nil)))
                  (dom2/props {:class ["edit"], :autofocus true})
                  (when (p/Unglitch. description) (.focus dom/node))))))
          (ui4/button (p/fn [] (p/server (transact! [[:db/retractEntity id]]) nil))
            (dom/props {:class ["destroy"]})))))))

#?(:clj
   (defn toggle-all! [db status]
     (let [ids    (query-todos db (if (= :done status) :active :done))]
       (map (fn [id] {:db/id id, :task/status status}) ids))))

(p/defn TodoList [state]
  (p/client
    (dom/div
      (dom/section {:class "main"}
        (let [active (p/server (todo-count db :active))
              all    (p/server (todo-count db :all))
              done   (p/server (todo-count db :done))]
          (ui4/checkbox (cond (= all done)   true
                              (= all active) false
                              :else          nil)
            (p/fn [v] (let [status (case v (true nil) :done, false :active)]
                        (p/server (transact! (toggle-all! db status)) nil)))
            (dom2/props {:class ["toggle-all"]})))
        (dom/label {:for "toggle-all"} "Mark all as complete")
        (dom/ul {:class "todo-list"}
          (p/for [id (p/server (sort (query-todos db (::filter state))))]
            (TodoItem. state id)))))))

(p/defn CreateTodo []
  (dom2/span (dom2/props {:class ["input-load-mask"]})
    (dom2/on-pending (dom2/props {:aria-busy true})
      (dom2/input
        (dom2/on "keydown"
          (p/fn [e]
            (when (= "Enter" (.-key e))
              (when-some [description (contrib.str/empty->nil (.-target.value e))]
                (p/server (transact! [{:task/description description, :task/status :active}]) nil)
                (set! (.-value dom/node) "")))))
        (dom2/props {:class ["new-todo"], :placeholder "What needs to be done?"})))))

(p/defn TodoApp [state]
  (dom/section {:class "todoapp"}
    (dom/header {:class "header"}
      (CreateTodo.))
    (when (p/server (pos? (todo-count db :all)))
      (TodoList. state))
    (dom/footer {:class "footer"}
      (TodoStats. state))))

(p/defn TodoMVC [state]
  (dom/div {:class "todomvc"}
    (dom/h1 "TodoMVC")
    (TodoApp. state)
    (dom/footer {:class "info"}
      (dom/p "Double-click to edit a todo"))))

(p/defn Diagnostics [state]
  (dom/h1 "Diagnostics")
  (dom/dl
    (dom/dt "count :all") (dom/dd (pr-str (p/server (todo-count db :all))))
    (dom/dt "query :all") (dom/dd (pr-str (p/server (query-todos db :all))))
    (dom/dt "state") (dom/dd (pr-str state))
    (dom/dt "delay") (dom/dd
                       (ui4/long (::delay state) (p/fn [v] (swap! !state assoc ::delay v))
                         (dom2/props {:step 1, :min 0, :style {:width :min-content}}))
                       (dom2/text " ms"))))

#?(:clj
   (defn slow-transact! [!conn delay tx]
     (try (Thread/sleep delay) ; artificial latency
          (d/transact! !conn tx)
          (catch InterruptedException _))))

(p/defn App []
  (p/client
    (let [state (p/watch !state)]
      (p/server
        (binding [db (p/watch !conn)
                  transact! (partial slow-transact! !conn (p/client (::delay state)))]
          (p/client
            (dom/link {:rel :stylesheet, :href "/todomvc.css"})
            (TodoMVC. state)
            #_(Diagnostics. state)))))))

(comment
  (todo-count @!conn :all)
  (todo-count @!conn :active)
  (todo-count @!conn :done)
  (query-todos @!conn :all)
  (query-todos @!conn :active)
  (query-todos @!conn :done)
  (d/q '[:find (count ?e) . :where [?e :task/status]] @!conn)
  )
