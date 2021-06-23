(ns user.todomvc
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as photon :refer [defnode main]]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

(defnode todos "" [status]
  ; active, completed, all
  (d/q '[:find [?e ...] :in $ ?needle :where
         [?e :task/name]]
       hf/*$*))

(s/fdef todos :args (s/cat :status keyword?))

(defnode header [& {:keys [::title]}]
  (dom/h1 title))

(defnode todo-table [xs]
  (dom/fragment
    (dom/table
      (dom/thead (for [x ["id" "name" "completed"]] (dom/td x)))
      (dom/tbody (for [x xs]
                   (dom/tr
                     (for [[k v] x]
                       ; toggle completion
                       ; checkbox-> datom
                       (dom/td v))))))
    (dom/span "Count: " (count xs))))

(defnode todo-detail "Details" [e]
  (hf/page
    ["Details"
     {e
      [:db/id
       :task/name
       :task/completed
       :task/description
       *]}]))

(defnode todo-new [e]
  (hf/page
    ["Enter a new todo"
     {e
      [:db/id
       :task/name]}]))

(defnode todo-list [status]
  (hf/page
    [(::hf/render header ::title "To Better Do")
     {((todos status) ::hf/render todo-table)
      [(-1 ::hf/new (todo-new %))
       (:db/id ::hf/a (todo-detail %))
       (:task/name ::hf/render dom/input)
       :task/completed]}]))

(defnode todomvc [route]
  )

(hf/serve (todomvc _))

; doing it this way gives you free routing
(def todomvc
  (hf/app
    ; docstring title
    {((todos _) ::hf/render todo-table)
     [
      (:db/id ::hf/a (todo-detail %))
      :task/name
      :task/completed]}

    {-1 [(::hf/new (todo-new %))]}                          ; same page


    {((todo-detail _) ::hf/render)
     [(::hf/a (todos _))
      :db/id
      :task/name
      :task/completed
      :todo/description]}))

(photon/main
  (loop [db (d/db *conn*)]
    (binding [hf/*$* db]
      (let [tx (todomvc ~@~(m/watch hf/*route*))]
        (recur (:db-after (d/with % tx)))))))
