(ns user.todomvc-hfql
  (:require [clojure.spec.alpha :as s]
            [datomic.api :as d]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :refer [defnode]]
            [hyperfiddle.photon-dom :as dom]))

(defnode todo-list [xs]
  (dom/fragment
    (dom/h1 "To Better Do, a distributed concurrent todo list application")
    (dom/table
      (dom/thead (for [x ["id" "name" "status"]] (dom/td x)))
      (dom/tbody (for [x xs]
                   (dom/tr
                     (for [[k v] x]
                       (dom/td v))))))
    (dom/span "Count: " (count xs))))

(defnode todos "" [status]
  (d/q '[:find [?e ...] :in $ ?status :where [?e :task/status ?status]] hf/*$* status))

(s/fdef todos :args (s/cat :status keyword?) :ret sequential?)

(defnode todo-new "Enter a new todo" [] -1)
(s/fdef todo-new :args (s/cat))

(defnode todo-detail [e] e)
(s/fdef todo-detail :args (s/cat :e ref?))

(def todomvc
  (hf/app
    {((todos .) ::hf/render todo-list)
     [(:db/id ::hf/a (todo-detail %))
      :task/status
      :task/description]}
    {((todo-detail .) ::hf/render)
     [:db/id
      :task/status
      :task/description]}
    {(todo-new)
     [:db/id
      :task/description]}))

(hf/serve todomvc)

(defnode page [[f & [status & args] :as route]]
  (case route
    [(hf/hfql
       {((todos status) ::hf/render todo-list)
        [(:db/id ::hf/a (todo-detail %))
         :task/status
         :task/description]})
     (hf/hfql
       {((todos status) ::hf/render todo-list)
        [(:db/id ::hf/a (todo-detail %))
         :task/status
         :task/description]})]))

(page ...)