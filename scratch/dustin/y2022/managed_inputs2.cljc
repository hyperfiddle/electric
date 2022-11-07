(comment

  (p/defn InputText [class placeholder value Create! Cancel!]
    (p/with-progress 100
                     (dom/span {:class "input-load-mask"
                                :aria-busy (not= p/state 0)}
                       (dom/input {:class class
                                   :placeholder placeholder
                                   :value (if p/busy (.-value dom/node) value)
                                   :on-keydown (p/fn [_]
                                                 (case (.-key e)
                                                   ("Enter") (Create!. (.-value dom/node))
                                                   ("Escape") (Cancel!.)
                                                   (do)))}))))

  )






; Enumeration of points we disagree on
; 1. L: explicit state machine is good, the successive states are essential complexity (part of the biz problem)
; 2. Leo re. Dustin design - does it scale to more complex components? (D: concretely which components?)
; Who's in charge of try/catch pending?
; How many states does the control transition through?
; What if the progress reporting is more complex than just a boolean?











; original
(p/defn InputText
  [class placeholder value Monitor Cancel]
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