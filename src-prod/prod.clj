(ns prod
  (:require clojure.string
            [contrib.datomic-m :as d]
            [missionary.core :as m]
            [electric-server-java8-jetty9 :refer [start-server!]]
            user-main))

(def host "0.0.0.0")
(def port 8080)

(when (clojure.string/blank? (System/getProperty "HYPERFIDDLE_ELECTRIC_SERVER_VERSION"))
  (throw (ex-info "$HYPERFIDDLE_ELECTRIC_SERVER_VERSION must be set in prod" {})))

(defn -main [& args]
  (println "Starting Electric server...")
  (def server (start-server! {:host host :port port :resources-path "public" :manifest-path "public/js/manifest.edn"}))
  (comment (.stop server))

  "Datomic Cloud (requires :scratch alias)"
  (try
    (def datomic-config {:server-type :dev-local :system "datomic-samples"})
    ; install prod globals
    (def datomic-client (d/client datomic-config))
    (def datomic-conn (m/? (d/connect datomic-client {:db-name "mbrainz-subset"})))
    (catch Exception _ "no datomic on classpath")))

