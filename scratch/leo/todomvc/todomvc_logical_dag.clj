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

(def todo-list
  (ifn [todos]
    [:ul
     (rfor [todo :db/id todos]
       ($ todo-item todo))]))

(defn main [!log !todos]
  (run-dag
    (let [vdom ($ todo-list @(m/watch !todos))]
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