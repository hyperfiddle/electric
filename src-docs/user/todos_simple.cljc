(ns user.todos-simple
  #?(:cljs (:require-macros user.todos-simple))
  (:require #?(:clj [datascript.core :as d])
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil))
(e/def db (e/watch !conn))

(e/defn TodoCreate []
  (dom/input (dom/props {:placeholder "Buy milk"})
    (dom/on "keydown" (e/fn [e]
                        (when (= "Enter" (.-key e))
                          (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                            (e/server (e/discard (d/transact! !conn [{:task/description v
                                                                     :task/status :active}])))
                            (set! (.-value dom/node) "")))))))

(e/defn TodoItem [id]
  (e/server
    (let [e (d/entity db id)
          status (:task/status e)]
      (e/client
        (dom/div
          (ui/checkbox
            (case status :active false, :done true)
            (e/fn [v]
              (e/server
                (d/transact! !conn [{:db/id id :task/status (if v :done :active)}])
                nil))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (e/server (:task/description e)))))))))

#?(:clj (defn todo-count [db] (count (d/q '[:find [?e ...] :in $ ?status
                                            :where [?e :task/status ?status]] db :active))))

(e/defn Todo-list []
  (e/client
    (dom/h1 (dom/text "minimal todo list"))
    (dom/div (dom/props {:class "todo-list"})
      (TodoCreate.)
      (dom/div {:class "todo-items"}
        (e/server
          (e/for [{:keys [db/id]} (->> (d/q '[:find [(pull ?e [:db/id :task/description]) ...] :where [?e :task/status]] db)
                                       (sort-by :task/description))]
            (TodoItem. id))))
      (dom/p (dom/props {:class "counter"})
        (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
        (dom/text " items left")))))