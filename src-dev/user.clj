(ns user
  "Photon app server build and run instructions (Clojure and ClojureScript).
  Start a REPL with `clj -A:dev`, or jack in with :dev alias.
  Default app is demo-healthcheck."
  (:refer-clojure :exclude [compile]))

; Don't slow down JVM REPL startup with :require for demos,
; (main) will cause the shadow build to load them and we share that JVM

(comment
  "Photon Clojure REPL entrypoint"
  (main)

  "turn on RCF (after all transitive dependencies have loaded)"
  (hyperfiddle.rcf/enable!)

  (shadow.cljs.devtools.api/compile :editor)

  "Switch examples"
  (user/browser-main! `user.demo-1-healthcheck/main)
  (user/browser-main! `user.demo-2-system-properties/main)
  (user/browser-main! `user.demo-3-webview/main)

  "ClojureScript REPL entrypoint"
  ; shadow server exports an repl, connect a second REPL instance to it (DO NOT REUSE JVM REPL it will fail weirdly)
  (shadow.cljs.devtools.api/repl :devkit)
  (type 1)
  )

(defmacro get-default-demo []
  (list 'quote (or (some-> (System/getenv "HF_DEMO") symbol)
                 `user.demo-0-entrypoint/main)))

(def cljs-eval (delay @(requiring-resolve 'shadow.cljs.devtools.api/cljs-eval)))
(def shadow-start! (delay @(requiring-resolve 'shadow.cljs.devtools.server/start!)))
(def shadow-watch (delay @(requiring-resolve 'shadow.cljs.devtools.api/watch)))
(def shadow-compile (delay @(requiring-resolve 'shadow.cljs.devtools.api/compile)))
(def shadow-release (delay @(requiring-resolve 'shadow.cljs.devtools.api/release)))
(def start-server! (delay (requiring-resolve 'server/start-server!)))

(defn browser-main! [photon-main-sym]
  ; Save the user the trouble of getting a CLJS repl to switch photon entrypoints
  (@cljs-eval :devkit (str `(println ::loading (quote ~photon-main-sym))) {})
  (@cljs-eval :devkit (str `(browser-main! (quote ~photon-main-sym))) {})
  (fn dispose [] (@cljs-eval :devkit `(user/stop!) {})))

(comment (@(requiring-resolve 'shadow.cljs.devtools.api/cljs-eval) :devkit (str "(println ::x)") {}))

(defn serve!
  "Start Photon app server"
  []
  (let [host "0.0.0.0"]
    (def server (@start-server! {:host host, :port 8080}))
    (println (str "\n👉 App available at http://" host ":" (-> server (.getConnectors) first (.getPort))
             "\n"))))

(defn main "CLJ main" [& args]
  "build and serve clojurescript assets"
  (@shadow-start!)                                          ; serves index.html as well
  (@shadow-watch :devkit)                                   ; depends on shadow server
  (serve!)
  (comment (.stop server)))

(defn compile []
  ; optimized artifact but with debug information available to find problems
  (@shadow-compile :devkit))

(defn release []
  ; optimized artifact but with debug information available to find problems
  (@shadow-release :app {:debug true}))

(defn boot! []
  (compile)
  (serve!))
