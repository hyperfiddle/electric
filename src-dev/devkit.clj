(ns devkit)

(require '[dev])
(require '[hyperfiddle.server :refer [start-server!]])
(require '[shadow.cljs.devtools.api :as shadow])
(require 'shadow.cljs.devtools.server)

(defn build-config [ns]
  {:build-id         :app
   :target           :browser
   :main             (str ns "/main")
   :devtools         {:watch-dir       "resources/public"
                      :watch-path      "/assets"
                      :hud             #{:errors :progress}
                      :ignore-warnings true ;; warnings don't prevent hot-reload
                      }
   :compiler-options {:output-feature-set    :es6
                      :infer-externs         :auto
                      :js-options            {:variable-renaming :off}
                      :anon-fn-naming-policy :unmapped}
   :build-options    {:cache-level :jars} ;; Recompile everything but jars.
   :output-dir       "resources/public/js"
   :asset-path       "/assets/js"
   :modules          {:shared {:entries [;; HACK: Shadow should take care of it
                                         'shadow.cljs.bootstrap.env
                                         'shadow.cljs.bootstrap.browser]}
                      :main   {:entries    [ns]
                               :depends-on #{:shared}
                               :append-js (str (munge ns) ".start_BANG_();")}}})

(defn run! [& {:keys [ns host port]
               :or   {host "localhost"
                      port 8080}
               :as   args}]
  (assert (some? ns) "\nPlease specify a namespace to load: `clj -X:devkit :ns your.namespace`. ")
  (println "\n# Compiling " ns "\n")
  (shadow.cljs.devtools.server/start!)
  (shadow/watch (build-config ns))
  (println "\n# Starting server on " host ":" port "\n")
  (start-server! {:host   "localhost"
                  :port   port
                  :scheme "http"})
  (println (str "\n# Your app: http://" host ":" port))
  (println "\n# You can connect a nREPL client to localhost:9001")
  (println "# Edit `" ns "` and save the file to recompile and live reload.\n"))