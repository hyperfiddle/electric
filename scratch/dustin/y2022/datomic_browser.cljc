(ns dustin.y2022.datomic-browser
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            #?(:clj datomic.client.api)
            #?(:clj [datomic.client.api.async :as d])
            [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.explorer :as explorer :refer [Explorer]]
            [user.datafy-fs #?(:clj :as :cljs :as-alias) fs])
  #?(:cljs (:require-macros dustin.y2022.datomic-browser)))

#?(:clj
   (defn query
     "Return a task running a datomic query asynchronously, and completing
     when all streamed results have been collected into a vector."
     [query & args]
     (p/chan->task (d/q {:query query, :args (vec args)}))))

#?(:clj (def datomic-client (datomic.client.api/client {:server-type :dev-local :system "datomic-samples"})))
#?(:clj (def datomic-conn (datomic.client.api/connect datomic-client {:db-name "mbrainz-subset"})))
#?(:clj (def db (d/db datomic-conn)))

(comment
  (->> (m/? (query '[:find (pull ?tx [:db/id :db/txInstant])
                     :where [?tx :db/txInstant]] db))
       (take 1))
  := [[#:db{:id 13194139533312, :txInstant #inst"1970-01-01T00:00:00.000-00:00"}]])

(p/defn App []
  (dom/div
    (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
    (p/server
      (binding
        [;explorer/Title (p/fn [m] (:db/id m))
         explorer/cols [:db/id :db/txInstant]
         explorer/search-attr :db/id
         explorer/Format (p/fn [x m a v] (pr-str v))]
        (Explorer. (->> (new (p/task->cp (query '[:find (pull ?tx [:db/id :db/txInstant])
                                                  :where [?tx :db/txInstant]] db)))
                        (take 5)
                        (map first))
                   "title")))))