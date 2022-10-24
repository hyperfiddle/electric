(ns prod
  (:require [contrib.datomic-m :as d]
            [missionary.core :as m]
            [hyperfiddle.photon-jetty-server :refer [start-server!]]
            shadow.cljs.devtools.api
            user-main))

(def host "0.0.0.0")
(def port 8080)

(defn build [& args]
  (println "Compiling shadow...")
  (shadow.cljs.devtools.api/compile :devkit))

(defn main [& args]
  (println "Starting Photon server...")
  (def server (start-server! {:host host :port port :resources-path "resources/public"}))
  (println (str "\n👉 App server available at http://" host ":" (-> server (.getConnectors) first (.getPort))
                "\n"))
  (comment (.stop server))

  "Datomic Cloud (requires :scratch alias)"
  (try
    (def datomic-config {:server-type :dev-local :system "datomic-samples"})
    ; install prod globals
    (def datomic-client (d/client datomic-config))
    (def datomic-conn (m/? (d/connect datomic-client {:db-name "mbrainz-subset"})))
    (catch Exception _ "no datomic on classpath")))