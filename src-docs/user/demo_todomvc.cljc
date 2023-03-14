(ns user.demo-todomvc
  "Requires -Xss2m to compile. The Electric compiler exceeds the default 1m JVM ThreadStackSize
  due to large macroexpansion resulting in false StackOverflowError during analysis."
  (:require
   contrib.str
   #?(:clj [datascript.core :as d])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil))       ; server
(e/def db)                                                  ; server
(e/def transact!) ; server
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

(e/defn Filter-control [state target label]
  (dom/a (dom/props {:class (when (= state target) "selected")})
    (dom/text label)
    (dom/on "click" (e/fn [_] (swap! !state assoc ::filter target)))))


(e/defn TodoStats [state]
  (let [active (e/server (todo-count db :active))
        done   (e/server (todo-count db :done))]
    (dom/div
      (dom/span (dom/props {:class "todo-count"})
        (dom/strong (dom/text active))
        (dom/span (dom/text " " (str (case active 1 "item" "items")) " left")))

      (dom/ul (dom/props {:class "filters"})
        (dom/li (Filter-control. (::filter state) :all "All"))
        (dom/li (Filter-control. (::filter state) :active "Active"))
        (dom/li (Filter-control. (::filter state) :done "Completed")))

      (when (pos? done)
        (ui/button (e/fn [] (e/server (when-some [ids (seq (query-todos db :done))]
                                          (transact! (mapv (fn [id] [:db/retractEntity id]) ids)) nil)))
          (dom/props {:class "clear-completed"})
          (dom/text "Clear completed " done))))))

(e/defn TodoItem [state id]
  (e/server
    (let [{:keys [:task/status :task/description]} (d/entity db id)]
      (e/client
        (dom/li
          (dom/props {:class [(when (= :done status) "completed")
                              (when (= id (::editing state)) "editing")]})
          (dom/div (dom/props {:class "view"})
            (ui/checkbox (= :done status) (e/fn [v]
                                              (let [status (case v true :done, false :active, nil)]
                                                (e/server (transact! [{:db/id id, :task/status status}]) nil)))
              (dom/props {:class "toggle"}))
            (dom/label (dom/text description)
                       (dom/on "dblclick" (e/fn [_] (swap! !state assoc ::editing id)))))
          (when (= id (::editing state))
            (dom/span (dom/props {:class "input-load-mask"})
              (dom/on-pending (dom/props {:aria-busy true})
                (dom/input
                  (dom/on "keydown"
                    (e/fn [e]
                      (case (.-key e)
                        "Enter" (when-some [description (contrib.str/blank->nil (-> e .-target .-value))]
                                  (case (e/server (transact! [{:db/id id, :task/description description}]) nil)
                                    (swap! !state assoc ::editing nil)))
                        "Escape" (swap! !state assoc ::editing nil)
                        nil)))
                  (dom/props {:class "edit" #_#_:autofocus true})
                  (dom/bind-value description) ; first set the initial value, then focus
                  (case description ; HACK sequence - run focus after description is available
                    (.focus dom/node))))))
          (ui/button (e/fn [] (e/server (transact! [[:db/retractEntity id]]) nil))
            (dom/props {:class "destroy"})))))))

#?(:clj
   (defn toggle-all! [db status]
     (let [ids    (query-todos db (if (= :done status) :active :done))]
       (map (fn [id] {:db/id id, :task/status status}) ids))))

(e/defn TodoList [state]
  (e/client
    (dom/div
      (dom/section (dom/props {:class "main"})
        (let [active (e/server (todo-count db :active))
              all    (e/server (todo-count db :all))
              done   (e/server (todo-count db :done))]
          (ui/checkbox (cond (= all done)   true
                             (= all active) false
                             :else          nil)
            (e/fn [v] (let [status (case v (true nil) :done, false :active)]
                        (e/server (transact! (toggle-all! db status)) nil)))
            (dom/props {:class "toggle-all"})))
        (dom/label (dom/props {:for "toggle-all"}) (dom/text "Mark all as complete"))
        (dom/ul (dom/props {:class "todo-list"})
          (e/for [id (e/server (sort (query-todos db (::filter state))))]
            (TodoItem. state id)))))))

(e/defn CreateTodo []
  (dom/span (dom/props {:class "input-load-mask"})
    (dom/on-pending (dom/props {:aria-busy true})
      (dom/input
        (dom/on "keydown"
          (e/fn [e]
            (when (= "Enter" (.-key e))
              (when-some [description (contrib.str/empty->nil (-> e .-target .-value))]
                (e/server (transact! [{:task/description description, :task/status :active}]) nil)
                (set! (.-value dom/node) "")))))
        (dom/props {:class "new-todo", :placeholder "What needs to be done?"})))))

(e/defn TodoMVC-UI [state]
  (dom/section (dom/props {:class "todoapp"})
    (dom/header (dom/props {:class "header"})
      (CreateTodo.))
    (when (e/server (pos? (todo-count db :all)))
      (TodoList. state))
    (dom/footer (dom/props {:class "footer"})
      (TodoStats. state))))

(e/defn TodoMVC-body [state]
  (dom/div (dom/props {:class "todomvc"})
    (dom/h1 (dom/text "TodoMVC"))
    (TodoMVC-UI. state)
    (dom/footer (dom/props {:class "info"})
      (dom/p (dom/text "Double-click to edit a todo")))))

(e/defn Diagnostics [state]
  (dom/h1 (dom/text "Diagnostics"))
  (dom/dl
    (dom/dt (dom/text "count :all")) (dom/dd (dom/text (pr-str (e/server (todo-count db :all)))))
    (dom/dt (dom/text "query :all")) (dom/dd (dom/text (pr-str (e/server (query-todos db :all)))))
    (dom/dt (dom/text "state")) (dom/dd (dom/text (pr-str state)))
    (dom/dt (dom/text "delay")) (dom/dd
                                   (ui/long (::delay state) (e/fn [v] (swap! !state assoc ::delay v))
                                     (dom/props {:step 1, :min 0, :style {:width :min-content}}))
                                   (dom/text " ms"))))

#?(:clj
   (defn slow-transact! [!conn delay tx]
     (try (Thread/sleep delay) ; artificial latency
          (d/transact! !conn tx)
          (catch InterruptedException _))))

(e/defn TodoMVC []
  (e/client
    (let [state (e/watch !state)]
      (e/server
        (binding [db (e/watch !conn)
                  transact! (partial slow-transact! !conn (e/client (::delay state)))]
          (e/client
            (dom/link (dom/props {:rel :stylesheet, :href "/todomvc.css"}))
            ; exclude #root style from todomvc-composed by inlining here
            (dom/element "style" (dom/text "body.hyperfiddle { width: 65vw; margin-left: auto; margin-right: auto; }"))
            (TodoMVC-body. state)
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
