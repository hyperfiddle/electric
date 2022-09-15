(ns user.datomic-browser
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            #?(:clj datomic.client.api)
            #?(:clj [datomic.client.api.async :as d])
            [missionary.core :as m]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [hyperfiddle.gridsheet :as-alias gridsheet]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :refer [tests ! %]]
            [user.util :refer [includes-str? pprint-str]])
  #?(:cljs (:require-macros user.datomic-browser)))

#?(:clj (def datomic-client (datomic.client.api/client {:server-type :dev-local :system "datomic-samples"})))
#?(:clj (def datomic-conn (datomic.client.api/connect datomic-client {:db-name "mbrainz-subset"})))
#?(:clj (def db (d/db datomic-conn)))

#?(:clj
   (defn transactions! [db pull-pattern]
     (m/sp (->> (m/? (p/chan->task
                       (d/q {:query '[:find (pull ?tx pattern)
                                      :in $ pattern
                                      :where [?tx :db/txInstant]]
                             :args [db pull-pattern]})))
                (map first)
                (sort-by :db/txInstant)
                reverse))))

#?(:clj (comment (take 3 (m/? (transactions! db [:db/id :db/txInstant])))
                 := [[#:db{:id _, :txInstant _}] & ?rest]))

#?(:clj (defn entity! [e db] (p/chan-read (d/pull db {:eid e :selector '[*]}))))

(comment
  (m/? (entity! 13194139533314 db))
  := #:db{:id 13194139533314, :txInstant _})

(p/def Navigate!)

(p/defn Nav-link [x label]
  (p/client
    (ui/element dom/a {::dom/href ""
                       ::ui/click-event (p/fn [e]
                                          (.preventDefault e)
                                          (p/server (Navigate!. x)))} label)))

(p/defn RecentTransactions []
  (binding [explorer/cols [:db/id :db/txInstant]
            explorer/Format (p/fn [m a v]
                              (case a
                                :db/id (Nav-link. [::tx v] v)
                                :db/txInstant (p/client (pr-str v) #_(.toLocaleDateString v))
                                (pr-str v)))]
    (Explorer.
      "Recent Txs"
      (new (p/task->cp (transactions! db [:db/id :db/txInstant])))
      {::dom/style {:height "calc((10 + 1) * 24px)"}
       ::explorer/page-size 10
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "10em auto"})))

#?(:clj
   (defn attributes [db pull-pattern]
     (m/sp (->> (m/? (p/chan->task
                       (d/q {:query '[:find (pull ?e pattern)
                                      :in $ pattern
                                      :where [?e :db/valueType _]]
                             :args [db pull-pattern]})))
                (map first)
                (sort-by :db/ident)))))

(comment (time (take 3 (m/? (attributes db [:db/ident])))))

(p/defn Attributes []
  (binding [explorer/cols [:db/ident :db/valueType :db/cardinality :db/unique :db/isComponent
                           #_#_#_#_:db/fulltext :db/tupleType :db/tupleTypes :db/tupleAttrs]
            explorer/Format (p/fn [m a v]
                              (case a
                                :db/ident (Nav-link. [::attribute v] v)
                                :db/valueType (some-> v :db/ident name)
                                :db/cardinality (some-> v :db/ident name)
                                :db/unique (some-> v :db/ident name)
                                (str v)))]
    (Explorer.
      "Attributes"
      (new (p/task->cp (attributes db explorer/cols)))
      {::dom/style {:height "calc((15 + 1) * 24px)"}
       ::explorer/page-size 15
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "auto 6em 4em 4em 4em"})))

#?(:clj
   (defn render-datoms [db !datoms]
     (m/sp (let [datoms (m/? !datoms) ; ?
                 ref-attr? (->> (m/? (p/chan->task (d/q {:query '[:find ?e :where [?e :db/valueType :db.type/ref]]
                                                         :args [db]})))
                                (map first) set)
                 datoms-id? (set (concat (map #(nth % 0) datoms)
                                         (map #(nth % 1) datoms)
                                         (->> datoms
                                              (filter #(ref-attr? (nth % 1)))
                                              (map #(nth % 2)))))
                 id->ident (into {} (m/? (p/chan->task (d/q {:query '[:find ?e ?v
                                                                      :in $ ?datoms-id?
                                                                      :where
                                                                      [?e :db/ident ?v]
                                                                      [(contains? ?datoms-id? ?e)]]
                                                             :args [db datoms-id?]}))))]
             (->> datoms (map (fn [[e a v t]]
                                {:e (get id->ident e e)
                                 :a (get id->ident a a)
                                 :v (get id->ident v v)
                                 :tx t})))))))

#?(:clj
   (defn tx-datoms [conn txid]
     ; https://docs.datomic.com/client-api/datomic.client.api.async.html#var-tx-range
     (->> (p/chan->flow (d/tx-range conn {:start txid, :end (inc txid)}))
          (m/eduction (take 1)) ; terminate flow after 1 tx (? why)
          (m/eduction (map :data))
          (m/reduce into []))))

(comment
  (take 3 (m/? (tx-datoms datomic-conn 13194139534022)))
  (time (take 3 (m/? (render-datoms db (tx-datoms datomic-conn 13194139534022)))))
  
  (time (take 3 (m/? (tx-datoms datomic-conn 13194139534018))))
  (time (take 3 (m/? (render-datoms db (tx-datoms datomic-conn 13194139534018))))))

#?(:clj (defn entity-datoms [db e] (p/chan->task (d/datoms db {:index :eavt, :components [e]}))))

(comment
  (m/? (entity-datoms db 1))
  (m/? (entity-datoms db :db/ident))
  (m/? (render-datoms db (entity-datoms db :db/ident))))

(p/defn EntityDetail [e]
  (binding [explorer/cols [:a :v :tx]
            explorer/Format (p/fn [m a v]
                              (case a
                                :a (Nav-link. [::attribute v] v)
                                :v (some-> v pr-str)
                                :tx (Nav-link. [::tx v] v)
                                :db/unique (some-> v :db/ident name)
                                (str v)))]
    (Explorer.
      (str "Entity detail: " e)
      (new (p/task->cp (render-datoms db (entity-datoms db e))))
      {::dom/style {:height "calc((20 + 1) * 24px)"}
       ::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em calc(100% - 15em - 9em) 9em"})))

#?(:clj (defn attr-datoms [db a] (p/chan->task (d/datoms db {:index :aevt, :components [a]}))))

(comment
  (m/? (attr-datoms db :db/ident))
  (m/? (render-datoms db (attr-datoms db :db/ident))))

(p/defn AttributeDetail [a]
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (p/fn [m a v]
                              (case a
                                :e (Nav-link. [::entity v] v)
                                :v (some-> v pr-str)
                                :tx (Nav-link. [::tx v] v)
                                (str v)))]
    (Explorer.
      (str "Attribute detail: " a)
      (new (p/task->cp (render-datoms db (attr-datoms db a))))
      {::dom/style {:height "calc((20 + 1) * 24px)"}
       ::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

(p/defn TxDetail [e]
  (binding [explorer/cols [:e :a :v :tx]
            explorer/Format (p/fn [m a v]
                              (case a
                                :e (Nav-link. [::entity v] v)
                                :a (Nav-link. [::attribute v] v)
                                :v (some-> v pr-str)
                                (str v)))]
    (Explorer.
      (str "Tx detail: " e)
      (new (p/task->cp (render-datoms db (tx-datoms datomic-conn e)))) ; global
      {::dom/style {:height "calc((20 + 1) * 24px)"}
       ::explorer/page-size 20
       ::explorer/row-height 24
       ::gridsheet/grid-template-columns "15em 15em calc(100% - 15em - 15em - 9em) 9em"})))

(defonce !route #?(:cljs nil :clj (atom [::summary])))
(comment
  (reset! !route [::summary])
  (reset! !route [::entity 13194139533314]))

(p/defn App []
  (binding [Navigate! (p/fn [x] (reset! !route x))]
    (p/client
      (dom/link {:rel :stylesheet, :href "user/datomic-browser.css"})
      (dom/div {:class "user-datomic-browser"}
        (dom/h1 "Datomic browser")
        (dom/div "Nav: "
          (Nav-link. [::summary] "home"))
        (p/server
          (let [[page x :as route] (p/watch !route)]
            (case page
              ::summary (do (RecentTransactions.) (Attributes.))
              ::attribute (AttributeDetail. x)
              ::tx (TxDetail. x)
              ::entity (EntityDetail. x)
              (str "no matching route: " (pr-str route)))))))))