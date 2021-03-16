(ns leo.todomvc.todomvc-logical-dag
  (:require [leo.hfdl :refer [ifn $]]
            [missionary.core :as m]))

(def button
  (ifn [done]
    [:button]))

(def editable-text
  (ifn [text]
    (let [!editing? (atom false)
          editing> (m/watch !editing?)
          #_#_editing? (mutable! false)]
      ; on click of the span, toggle editing
      (if @editing>
        [:input {:type "text" :value text}]
        [:span text]))))

(def todo-item
  (ifn [{:keys [id done description]}]
    [:li
     [:checkbox]
     ($ editable-text description)
     ($ button done)
     ]))

(defn query-todos [db]
  (datomic.api/q
    '[:find [(pull ?e [:db/id :description :done]) ...] :where [?e :todo/id]]
    db))

(def todo-list
  (ifn [db]
    [:ul
     ; put the network transfer inside the rfor body
     (>> ($ todo-item `(identity (<< :db/id (query-todos db)))))
     #_(rfor [% :db/id (query-todos db)] ($ todo-item `(identity %)))]))

(defn main [!log !todos]
  (run-dag
    (let [vdom ($ todo-list db)]
      (swap! !log conj vdom))))

(tests

  (def !log (atom []))
  (def !todos (atom []))
  (def process (main !log !todos))
  (process (partial prn :success) (partial prn :failure))

  ;(replay! reactor [[]])
  ; the programmer can decide if state is part of the domain
  ; or local to the component.
  ; In both cases they are addressable externally

  (reset !state
    {#_#_
     :todos-enriched [{:id 1 :ui/editing true}
                      {:id 2 :ui/editing false}]
     :todos
     [{:id 1 :done false :description "a" #_#_::ui/editing true}
      {:id 2 :done true :description "b"}]})
  @!log := [:ul
            [:li [:checkbox {:checked false}]
             [:span "a"]
             [:button]]
            [:li [:checkbox {:checked true}]
             [:input {:type "text" :value "b"}]
             [:button]]]
  )

(ifn [db]
  (datomic.api/with db [:db/add [:db/id 2] ::ui/editing true]))