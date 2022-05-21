(ns user
  "Photon app server build and run instructions (Clojure and ClojureScript).
  Start a REPL with `clj -A:dev`, or jack in with :dev alias.
  Default app is demo-healthcheck."
  (:require #?(:cljs goog.object)
            user.demo-healthcheck
            user.demo-system-properties
            user.demo-webview
            user.demo-button
            user.demo-counter
            user.app-starter
            wip.demo-two-clocks
            #_wip.orders-ui
            #_wip.hytradboi
            ))


(comment
  "Photon Clojure REPL entrypoint"
  (main)

  "Switch examples"
  (user/browser-main! `user.demo-healthcheck/main)
  (user/browser-main! `user.demo-system-properties/main)
  (user/browser-main! `user.demo-webview/main)

  "ClojureScript REPL entrypoint"
  ; shadow server exports an repl, connect a second REPL instance to it (DO NOT REUSE JVM REPL it will fail weirdly)
  (shadow.cljs.devtools.api/repl :devkit)
  (type 1)
  )

#?(:cljs (defn runtime-resolve [exported-qualified-sym]
           (let [path-s        (str (munge (symbol (namespace exported-qualified-sym)))
                                    "." (munge (name exported-qualified-sym)))
                 path-segments (clojure.string/split path-s ".")]
             (goog.object/getValueByKeys js/window (clj->js path-segments)))))

#?(:cljs (defonce user-photon-main `user.demo-healthcheck/main)) ; lazy resolve
#?(:cljs (defonce reactor nil))                             ; save for debugging

(defn ^:dev/after-load ^:export start! []
  #?(:cljs (when user-photon-main
             (set! reactor ((runtime-resolve user-photon-main) ; Photon main recompiles every reload, must re-resolve it
                            #(js/console.log "Reactor success:" %)
                            #(js/console.error "Reactor failure:" %))))))

(defn ^:dev/before-load stop! []
  #?(:cljs (do (when reactor (reactor) #_"teardown")
               (set! reactor nil))))

#?(:clj  (defn browser-main! [photon-main-sym]
           ; Save the user the trouble of getting a CLJS repl to switch photon entrypoints
           (let [cljs-eval @(requiring-resolve 'shadow.cljs.devtools.api/cljs-eval)]
             (cljs-eval :devkit (str `(println ::loading (quote ~photon-main-sym))) {})
             (cljs-eval :devkit (str `(browser-main! (quote ~photon-main-sym))) {})
             (fn dispose [] (cljs-eval :devkit `(user/stop!) {}))))

   :cljs (defn browser-main! [photon-main-sym]
           ;(println ::received-reload-command photon-main-sym (type photon-main-sym))
           (set! user-photon-main photon-main-sym) (stop!) (start!)))

(comment
  (@(requiring-resolve 'shadow.cljs.devtools.api/cljs-eval) :devkit (str "(println ::x)") {})
  )

#?(:clj
   (defn main "CLJ main" [& args]
     "build and serve clojurescript assets"
     (@(requiring-resolve 'shadow.cljs.devtools.server/start!)) ; serves index.html as well
     (@(requiring-resolve 'shadow.cljs.devtools.api/watch) :devkit) ; depends on shadow server
     #_(@(requiring-resolve 'shadow.cljs.devtools.api/compile) :devkit)
     #_(@(requiring-resolve 'shadow.cljs.devtools.api/release) :devkit)

     "Start Photon app server"
     (def server (@(requiring-resolve 'hyperfiddle.photon/start-websocket-server!) {:host "localhost" :port 8081}))
     (comment (.stop server))))
