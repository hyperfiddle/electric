(ns devkit
  #?(:clj (:require [dev]
                    [hyperfiddle.photon :as p]
                    [shadow.cljs.devtools.api :as shadow]
                    [shadow.cljs.devtools.server :as shadow-server])))

(defn build-config [sym]
  (let [ns (symbol (namespace sym))]
    {:build-id      :app
     :target        :browser
     :devtools      {:watch-dir       "resources/public" ; live reload CSS
                     :hud             #{:errors :progress}
                     :ignore-warnings true ; warnings don't prevent hot-reload
                     }
     :build-options {:cache-level :jars} ; Recompile everything but jars.
     :output-dir    "resources/public/js"
     :asset-path    "/js"
     :modules       {:main {:entries   ['devkit ns]
                            :append-js (str "devkit.main_fn = function(){return " (munge ns) "." (munge (name sym)) "};"
                                         "devkit.start_BANG_();")}}}))

(def reactor)
(def main-fn)

(defn ^:dev/before-load stop! []
  #?(:cljs (do (when reactor (reactor)) ; teardown
             (set! reactor nil))))

(defn ^:dev/after-load ^:export start! []
  #?(:cljs (set! reactor ((main-fn) js/console.log js/console.error))))

#?(:clj
   (defn main [& {:keys [main]}]
     (assert (qualified-symbol? main) "\nPlease specify a main function to run: `clj -X:devkit :main your.namespace/main`. ")

     (println "\n# Compiling " (namespace main) "\n")
     (shadow-server/start!)
     (shadow/watch (build-config main)) ;; Assets are served by shadow
     (p/start-server! {:host "localhost", :port 8081}) ;; Websocket only

     (println (str "\n# Your app: http://localhost:8080"))
     (println "\n# You can connect a nREPL client to localhost:9001")
     (println (str "# Edit `" (namespace main) "` and save the file to recompile and live reload.\n"))))