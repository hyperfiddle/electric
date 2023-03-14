(ns prod
  (:require [contrib.datomic-m :as d]
            [missionary.core :as m]
            taoensso.timbre
            clojure.string
            [hyperfiddle.electric-jetty-server :refer [start-server!]]
            user-main))

(def host "0.0.0.0")
(def port 8080)

(defn -main [& args]
  (println "Starting Photon server...")
  (def server (start-server! {:host host :port port :resources-path "public" :manifest-path "public/js/manifest.edn"}))
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


(taoensso.timbre/merge-config!
  {:min-level :warn
   :timestamp-opts {:pattern "HH:mm:ss.SSS"}
   :output-fn (fn output-fn
                ([data] (output-fn nil data))
                ([opts {:keys [level ?err timestamp_ ?line ?ns-str msg_]}]
                 (let [color ({:info :blue, :warn :yellow, :error :red, :fatal :red} level)]
                   (str (force timestamp_) " " (taoensso.timbre/color-str color (clojure.string/upper-case (name level))) " [" ?ns-str ":" ?line "] - "  (force msg_)
                     (when ?err
                       (str "\n" (taoensso.timbre/stacktrace ?err opts)))))))})
