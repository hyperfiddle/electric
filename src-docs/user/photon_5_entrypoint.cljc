(ns user.photon-5-entrypoint
  "This is a self-contained example; run it with:
   clj -A:devkit -X user.photon-5-entrypoint/main"
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            #?(:clj shadow.cljs.devtools.server)
            #?(:clj shadow.cljs.devtools.api)))


(p/defn Foo []
  (dom/div
    (dom/text ~@ (pr-str (type 1)))))                       ; ~@ marks client/server transfer

(p/defn App []
  (binding [dom/parent (dom/by-id "root")]
    (dom/div
      (dom/attribute "id" "main")
      (dom/class "browser")
      (dom/div
        (dom/class "view")
        (Foo.)))))

(def photon-entrypoint #?(:cljs (p/client (p/main (App.))))) ; Photon entrypoint is on the client

#?(:cljs (def reactor))                                     ; save for debugging
#?(:cljs (defn ^:dev/after-load ^:export start! []
           (set! reactor (photon-entrypoint js/console.log js/console.error))))
#?(:cljs (defn ^:dev/before-load stop! []
           (do (when reactor (reactor) #_"dispose") (set! reactor nil))))

#?(:cljs (start!))

#?(:clj
   (defn main [& args]
     ; assert node_modules
     (shadow.cljs.devtools.server/start!)                   ; shadow serves nrepl and browser assets including entrypoint
     (shadow.cljs.devtools.api/watch
       {:build-id      :app
        :target        :browser
        :devtools      {:watch-dir "resources/public"}      ; live reload CSS
        :build-options {:cache-level :jars}                 ; recompile everything but jars
        :output-dir    "resources/public/js"
        :asset-path    "/js"
        :modules       {:main {:entries ['user.photon-5-entrypoint]}}})
     (p/start-websocket-server! {:host "localhost" :port 8081})
     (println (str "\n" "http://localhost:8080"))))
