(ns dustin.todomvc
  (:require [datascript.core :as d]
            [missionary.core :as m]
            [minitest :refer [tests]]
            [hfdl.lang :refer [dataflow debug! heap-dump]]
            [hfdl.lib :refer [reactive-for]]))

(defn result [program process]
  (get (heap-dump process) [(:result program)]))

(def ^:dynamic *$*)

(def schema
  {:task/name      {#_#_:db/valueType :db.type/string :db/cardinality :db.cardinality/one #_#_:db/unique :db.unique/identity}
   :task/completed {#_#_:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}})

(declare fixtures)

(defn init-datascript []
  (let [$ (->
            (d/create-conn schema)
            d/db
            fixtures)]
    (alter-var-root #'*$* (constantly $))
    :ok))

(defn fixtures [$]
  (-> $
    (d/with [{:task/name "buy milk" :task/completed true}
             {:task/name "feed baby"}
             {:task/name "do laundry"}])
    :db-after))

(def q '[:find [?e ...] :where [?e :task/name]])

(defn query-todos [>db]
  (dataflow (sort (d/q q @>db))))

(defn todo-item [>db id]
  (dataflow
    [:li
     [:input {:type "text" :value (:task/name (d/entity @>db id))}]
     [:input {:type "checkbox" :checked (boolean (:task/completed (d/entity @>db id)))}]
     #_($ button done)]))

(defn todo-list-view [>db]
  (dataflow
    (into [:ul]
      @(reactive-for (partial todo-item >db) (query-todos >db)))))

(defn app [>db]
  (dataflow
    (let [db @>db]
      @(todo-list-view ~db))))

(comment
  (init-datascript)
  (def !db (atom *$*))
  (def dag (app (m/watch !db)))
  (def p (debug! dag))
  (result dag @p)
  := [:ul
      [:li
       [:input {:value "buy milk", :type "text"}]
       [:input {:type "checkbox", :checked true}]]
      [:li
       [:input {:value "feed baby", :type "text"}]
       [:input {:type "checkbox", :checked nil}]]
      [:li
       [:input {:value "do laundry", :type "text"}]
       [:input {:type "checkbox", :checked nil}]]]

  (reset! !db (:db-after (d/with *$* [[:db/add "four" :task/name "code"]
                                      [:db/add "four" :task/completed true]
                                      #_[:db/retractEntity 2]])))

  (result dag @p)
  ;:= [:ul
  ;    [:li [:input {:value "buy milk", :type "text"}] [:input {:type "checkbox", :checked true}]]
  ;    [:li [:input {:value "feed baby", :type "text"}] [:input {:type "checkbox", :checked nil}]]
  ;    [:li [:input {:value "do laundry", :type "text"}] [:input {:type "checkbox", :checked nil}]]
  ;    [:li [:input {:value "code", :type "text"}] [:input {:type "checkbox", :checked true}]]]

  (reset! !db (:db-after (d/with *$* [#_[:db/add "five" :task/name "five"]
                                      [:db/retractEntity 2]])))

  )
