(ns leo.todomvc
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [user.demo-5-todomvc :as t]
            [datascript.core :as d])
  (:import hyperfiddle.photon.Pending))

(p/defn InputText "
Todo item creator widget. Argument is a p/fn taking todo description and returning action progress as a boolean
(truthy if done).
" [class placeholder value Monitor Cancel]
  `(p/with-state :idle
     (dom/h :span {:class     "input-load-mask"
                   :aria-busy (not= :idle p/state)}
       (dom/h :input
         {:class       class
          :placeholder placeholder
          :value       (case p/state :idle value (.-value dom/node))
          :on-keydown  (fn [e]
                         (case (.-key e)
                           ("Enter") (p/change! {} :enter)
                           ("Escape") (p/change! {} :escape)
                           (do)))}
         (case p/state
           :idle (do)
           :enter (when (Monitor. (.-value dom/node))
                    (p/change! {} :idle))
           :escape (when (Cancel.)
                     (p/change! {} :idle)))
         dom/node))))

(p/defn CheckBox [class checked Monitor]
  (p/with-state false
    (dom/h :input {:type      "checkbox"
                   :class     class
                   :checked   checked
                   :aria-busy p/state
                   :on-input  (fn [_] (p/change! {} true))}
      (when p/state
        (when (Monitor. (.-checked dom/node))
          (p/change! {} false)))
      dom/node)))

(p/defn Button [class Monitor text]
  (p/with-state false
    (dom/h :button
      {:class class
       :aria-busy p/state
       :on-click (fn [_] (p/change! {} true))}
      (when p/state
        (when (Monitor.)
          (p/change! {} false)))
      text)))

(p/defn TodoItem [state id]
  (p/server
    (let [{:keys [:task/status :task/description]} (d/entity t/db id)]
      (p/client
        (dom/h :li
          {:class [(when (= :done status) "completed")
                   (when (= id (::editing state)) "editing")]}
          (dom/h :div {:class "view"}
            (CheckBox. "toggle"
              (= :done status)
              (p/fn [checked]
                (try (p/server
                       (d/transact! t/db
                         [{:db/id id
                           :task/status (if checked :done :active)}]))
                     (catch Pending _))))
            (dom/h :label {:on-dblclick (fn [_] (swap! t/!state assoc ::editing id))} description))
          (when (= id (::editing state))
            ;; TODO focus requires access to dom element
            (.focus (InputText. "edit" "" description
                      (p/fn [description]
                        (when (try (p/server (d/transact! t/db [{:db/id id, :task/description description}]))
                                   (catch Pending _))
                          (swap! t/!state dissoc ::editing)))
                      (p/fn [] (swap! t/!state dissoc ::editing)))))
          (Button. "destroy"
            (p/fn []
              (try (p/server (d/transact! t/db [[:db/retractEntity id]]))
                   (catch Pending _))) nil))))))

(p/defn TodoList [s]
  (p/client
    (dom/h :div
      (dom/h :section {:class "main"}
        (CheckBox. "toggle-all"
          (= (p/server (t/todo-count t/db :all))
            (p/server (t/todo-count t/db :done)))
          (p/fn [checked]
            (p/server
              (try
                (d/transact! t/!conn
                  (t/toggle-all! t/db
                    (if checked :done :active)))
                (catch Pending _)))))
        (dom/h :label {:for "toggle-all"} "Mark all as complete")
        (dom/h :ul {:class "todo-list"}
          (p/for [id (p/server (sort (t/query-todos t/db (::filter s))))]
            (TodoItem. s id)))))))

(p/defn Filter-control [state target label]
  (dom/h :a {:class    (when (= (::filter state) target) "selected")
             :on-click (fn [_] (swap! t/!state assoc ::filter target))}
    label))

(p/defn TodoStats [state]
  (let [active (p/server (t/todo-count t/db :active))
        done   (p/server (t/todo-count t/db :done))]
    (dom/h :div
      (dom/h :span {:class "todo-count"}
        (dom/h :strong active)
        (dom/h :span " " (str (case active 1 "item" "items")) " left"))
      (dom/h :ul {:class "filters"}
        (dom/h :li (Filter-control. state :all "All"))
        (dom/h :li (Filter-control. state :active "Active"))
        (dom/h :li (Filter-control. state :done "Completed")))
      (when (pos? done)
        (Button. "clear-completed"
          (p/fn []
            (p/server (when-some [ids (seq (t/query-todos t/db :done))]
                        (d/transact! t/db (mapv (fn [id] [:db/retractEntity id]) ids)))))
          (str "Clear completed " done))))))

(p/defn TodoApp [state]
  (dom/h :section {:class "todoapp"}
    (dom/h :header {:class "header"}
      (InputText. "new-todo" "What needs to be done?" ""
        (p/fn [description]
          (try
            (p/server
              (d/transact! t/!conn
                [{:task/description description
                  :task/status      :active}]))
            (catch Pending _)))
        (p/fn [] true)))
    (when (p/server (pos? (t/todo-count t/db :all)))
      (TodoList. state))
    (dom/h :footer {:class "footer"}
      (TodoStats. state))))
