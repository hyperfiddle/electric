(ns user.todomvc
  (:require [datascript.core :as d]
            [hfdl.lang :as r :refer [defnode node]]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests ! %]]
            [missionary.core :as m]))

(r/defn todos [db status]
  (d/q '[:find [?e ...] :in $ ?status
         :where [?e :task/status ?status]]
       db status))

(r/defn root [db]
  (element :fragment
    (element :h1 "To Better Do, a distributed concurrent todo list application")
    (let [status (element :input "")
          es (r/$ todos db status)]
      (element :table
        (element :thead (r/for [x ["id" "status" "name"]] (element :td x)))
        (element :tbody (r/for [e es]
                     (let [{:keys [:db/id
                                   :task/status
                                   :task/description]} (d/entity db e)]
                       (element :tr e
                         (element :td :db/id id)
                         (element :td :task/status (element :checkbox status))
                         (element :td :task/description (element :input description)))))))
      (element :span "Count: " (count es)))))

(tests
  (def !db (atom (d/db *conn*)))
  (def dispose
    (r/run
      (r/binding [dom/parent (js/document.getElementById "#root")]
        (let [tx (r/$ root ~(m/watch !db))]
          (! tx)
          (swap! db #(:db-after (d/with % tx)))
          #_(d/transact *conn* tx)))))
  % := _
  (dispose))
