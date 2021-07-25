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
  (r/$ dom/fragment
    (r/$ dom/h1 "To Better Do, a distributed concurrent todo list application")
    (let [status (r/$ dom/input "")
          es (r/$ todos db status)]
      (r/$ dom/table
        (r/$ dom/thead (r/for [x ["id" "status" "name"]] (r/$ dom/td x)))
        (r/$ dom/tbody (r/for [e es]
                     (let [{:keys [:db/id
                                   :task/status
                                   :task/description]} (d/entity db e)]
                       (r/$ dom/tr e
                         (r/$ dom/td :db/id id)
                         (r/$ dom/td :task/status (r/$ dom/checkbox status))
                         (r/$ dom/td :task/description (r/$ dom/input description)))))))
      (r/$ dom/span "Count: " (count es)))))

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
