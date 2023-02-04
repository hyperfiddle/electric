(ns user.todos-simple
  #?(:cljs (:require-macros user.todos-simple))
  (:require #?(:clj [datascript.core :as d])
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-ui4 :as ui]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil))
(p/def db (p/watch !conn))

(p/defn TodoCreate []
  (dom/input (dom/props {:placeholder "Buy milk"})
    (dom/on "keydown" (p/fn [e]
                        (when (= "Enter" (.-key e))
                          (when-some [v (contrib.str/empty->nil (p/fuse (-> ^js e .-target .-value)))]
                            (p/server (p/discard (d/transact! !conn [{:task/description v
                                                                     :task/status :active}])))
                            (set! (.-value dom/node) "")))))))

(p/defn TodoItem [id]
  (p/server
    (let [e (d/entity db id)
          status (:task/status e)]
      (p/client
        (dom/div
          (ui/checkbox
            (case status :active false, :done true)
            (p/fn [v]
              (p/server
                (d/transact! !conn [{:db/id id :task/status (if v :done :active)}])
                nil))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (p/server (:task/description e)))))))))

#?(:clj (defn todo-count [db] (count (d/q '[:find [?e ...] :in $ ?status
                                            :where [?e :task/status ?status]] db :active))))

(p/defn Todo-list []
  (p/client
    (dom/h1 (dom/text "minimal todo list"))
    (dom/div (dom/props {:class "todo-list"})
      (TodoCreate.)
      (dom/div {:class "todo-items"}
        (p/server
          (p/for [{:keys [db/id]} (->> (d/q '[:find [(pull ?e [:db/id :task/description]) ...] :where [?e :task/status]] db)
                                       (sort-by :task/description))]
            (TodoItem. id))))
      (dom/p (dom/props {:class "counter"})
        (dom/span (dom/props {:class "count"}) (dom/text (p/server (todo-count db))))
        (dom/text " items left")))))