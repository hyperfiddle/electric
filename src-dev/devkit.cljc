(ns devkit
  "reusable entrypoint for Photon client/server apps"
  #?(:clj (:require [hyperfiddle.photon :as p]
                    [shadow.cljs.devtools.api :as shadow]
                    [shadow.cljs.devtools.server :as shadow-server])))

(defn build-config [sym]
  (let [ns (symbol (namespace sym))]
    {:build-id      :app
     :target        :browser
     :devtools      {:watch-dir       "resources/public"    ; live reload CSS
                     :hud             #{:errors :progress}
                     :ignore-warnings true}                 ; warnings don't prevent hot-reload
     :build-options {:cache-level :jars}                    ; recompile everything but jars
     :output-dir    "resources/public/js"
     :asset-path    "/js"
     :modules       {:main {:entries   ['devkit ns]
                            :append-js (str "devkit.main = " (munge ns) "." (munge (name sym)) ";"
                                            "devkit.start_BANG_();")}}}))

#?(:cljs (def main))                                        ; assigned above with :append-js
#?(:cljs (def reactor))                                     ; save for debugging
(defn ^:dev/after-load ^:export start! [] #?(:cljs (set! reactor (main js/console.log js/console.error))))
(defn ^:dev/before-load stop! [] #?(:cljs (do (when reactor (reactor) #_"teardown") (set! reactor nil))))

#?(:clj
   (defn main [& {:keys [main]}]
     (assert (qualified-symbol? main) "\nUsage: `clj -X:devkit :main your.namespace/main`")
     (shadow-server/start!)                                 ; shadow serves nrepl and browser assets including entrypoint
     (shadow/watch (build-config main))
     (def server (p/start-websocket-server! {:host "localhost" :port 8081}))
     (comment (.stop server))
     server))

;#?(:cljs (start!)) -- delay until after main_fn assignment
