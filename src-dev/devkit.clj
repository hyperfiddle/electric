(ns devkit
  (:require [dev]
            [hyperfiddle.photon :as p]
            [shadow.cljs.devtools.api :as shadow]
            [shadow.cljs.devtools.server :as shadow-server]))

(defn build-config [ns]
  {:build-id      :app
   :target        :browser
   :devtools      {:watch-dir "resources/public"} ;; live reload CSS
   :build-options {:cache-level :jars} ;; Recompile everything but jars.
   :output-dir    "resources/public/js"
   :asset-path    "/js"
   :modules       {:main {:entries   [ns]
                          :append-js (str (munge ns) ".start_BANG_();")}}})

(defn main [& {:keys [ns]}]
  (assert (some? ns) "\nPlease specify a namespace to load: `clj -X:devkit :ns your.namespace`. ")
  
  (println "\n# Compiling " ns "\n")
  (shadow-server/start!)
  (shadow/watch (build-config ns)) ;; Assets are served by shadow
  (p/start-server! {:host "localhost", :port 8081}) ;; Websocket only

  (println (str "\n# Your app: http://localhost:8080"))
  (println "\n# You can connect a nREPL client to localhost:9001")
  (println (str "# Edit `" ns "` and save the file to recompile and live reload.\n")))