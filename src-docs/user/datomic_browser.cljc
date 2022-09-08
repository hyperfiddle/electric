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

#?(:clj (comment (m/? (transactions! db [:db/id :db/txInstant]))
                 := [[#:db{:id _, :txInstant _}] & ?rest]))

#?(:clj (defn entity! [e db] (p/chan-read (d/pull db {:eid e :selector '[*]}))))

(comment
  (m/? (entity! 13194139533314 db))
  := #:db{:id 13194139533314, :txInstant _})

(p/defn Nav-link [x label]
  (p/client
    (ui/element dom/a {::dom/href ""
                       ::ui/click-event (p/fn [e]
                                          (.preventDefault e)
                                          (p/server (explorer/Navigate!. x)))} label)))

(p/defn RecentTransactions []
  (binding [explorer/cols [:db/id :db/txInstant]
            explorer/Search? (p/fn [m s] (includes-str? (:db/id m) s))
            explorer/Format (p/fn [m a v]
                              (case a
                                :db/id (Nav-link. [::entity v] v)
                                :db/txInstant (p/client (.toLocaleDateString v))
                                (pr-str v)))]
    (time (Explorer.
            "Recent Txs"
            (new (p/task->cp (transactions! db [:db/id :db/txInstant])))
            {::gridsheet/grid-template-columns "10em 10em"}))))

#?(:clj
   (defn attributes [db pull-pattern]
     (m/sp (->> (m/? (p/chan->task
                       (d/q {:query '[:find (pull ?e pattern)
                                      :in $ pattern
                                      :where [?e :db/valueType _]]
                             :args [db pull-pattern]})))
                (map first)
                (sort-by :db/ident)
                #_(paginate limit page)))))

(comment (time (m/? (attributes db [:db/ident] 3 0))))

(p/defn Attributes []
  (binding [explorer/cols [:db/ident :db/valueType :db/cardinality :db/unique]
            explorer/Search? (p/fn [m s] (includes-str? (:db/ident m) s))
            explorer/Format (p/fn [m a v]
                              (case a
                                :db/ident (Nav-link. [::entity v] v)
                                :db/valueType (some-> v :db/ident name)
                                :db/cardinality (some-> v :db/ident name)
                                :db/unique (some-> v :db/ident name)
                                (str v)))]
    (Explorer.
      "Attributes"
      (new (p/task->cp (attributes db explorer/cols)))
      {::gridsheet/grid-template-columns "auto 8em 8em 8em"})))

(p/defn Entity [x]
  (p/client
    (dom/dl
      (p/server
        (p/for [[k x] (datafy (new (p/task->cp (entity! x db))))]
          (p/client
            (dom/dt (pr-str k))
            (dom/dd (pr-str (p/server (datafy (nav x k x)))))))))))

(def !route #?(:clj (atom [::transactions])))
(comment
  (reset! !route [::transactions])
  (reset! !route [::entity 13194139533314]))

(p/defn App []
  (p/server
    (binding [explorer/Navigate! (p/fn [x] (p/server (reset! !route x)))]
      (p/client
        (dom/div {:class "photon-demo-explorer"}
          (dom/h1 "Explorer")
          (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
          (dom/div "Nav: "
            (Nav-link. [::transactions] "txs") " "
            (Nav-link. [::attributes] "attrs"))
          (p/server
            (let [[page x] (p/watch !route)]
              (case page
                ::transactions (RecentTransactions.)
                ::attributes (Attributes.)
                ::entity (Entity. x)
                (str "no matching route, page: " page)))))))))