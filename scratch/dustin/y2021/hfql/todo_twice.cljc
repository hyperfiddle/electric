(ns user.todomvc
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            [hyperfiddle.api :as hf :refer [hfql]]
            [hyperfiddle.photon :as photon :refer [defnode main]]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

(defnode todos "doc" [& [status]]
  ; active, completed, all
  (d/q '[:find [?e ...] :in $ ?needle :where
         [?e :task/name]]
       hf/*$*))

(defnode todo-table [xs]
  (dom/fragment
    (dom/table
      (dom/thead (for [x ["id" "name" "completed"]] (dom/td x)))
      (dom/tbody (for [x xs]
                   (dom/tr
                     (for [[k v] x]
                       (dom/td v))))))
    (dom/span "Count: " (count xs))))

(defnode TodoTwice [{:keys [needle1 needle2]}]
  (hfql
    [{((todos _) ::hf/render todo-table)
      [:db/id
       (:task/name ::hf/render dom/input)
       :task/completed]}
     {((todos _) ::hf/render todo-table)
      [:db/id
       (:task/name ::hf/render dom/input)
       :task/completed]}]))

(s/fdef TodoTwice :args (s/cat :needle1 string? :needle2 string?))
(s/fdef todos :args (s/cat :status keyword?))

; Does HF have a page abstraction or model

(def todomvc-app
  (hf/app
    [{((todos _) ::hf/render todo-table)
      [(:db/id ::hf/a todo-detail)
       (:task/name ::hf/render dom/input)
       :task/completed]}
     {(todo-detail _)
      [:db/id
       :task/name
       :task/completed]}]))





















