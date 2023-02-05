

; user.cljs
(ns ;^:dev/always ; recompile Photon entrypoint when Photon source changes
  user
  (:require clojure.string
            goog.object
            hyperfiddle.photon
            hyperfiddle.photon-dom-test
            hyperfiddle.rcf
            user-main)
  (:require-macros [user :refer [get-main]]))

(defn runtime-resolve [exported-qualified-sym]
  (assert (qualified-symbol? exported-qualified-sym))
  (let [path-s        (str (munge (symbol (namespace exported-qualified-sym)))
                           "." (munge (name exported-qualified-sym)))
        path-segments (clojure.string/split path-s ".")]
    (goog.object/getValueByKeys js/window (clj->js path-segments))))

(def ^:export photon-main (hyperfiddle.photon/boot (user-main/Main.)))
(defonce user-photon-main (get-main user/photon-main)) ; lazy resolve, dev user can alter at REPL
(defn set-main [s] (set! user-photon-main (symbol s)))
(defonce reactor nil)

(defn ^:dev/after-load ^:export start! [main]
  (when (or user-photon-main main)
    (set! reactor ((runtime-resolve (or main user-photon-main))       ; Photon main recompiles every reload, must re-resolve it
                   #(js/console.log "Reactor success:" %)
                   #(js/console.error "Reactor failure:" %))))
  (hyperfiddle.rcf/enable!))

(defn ^:dev/before-load stop! []
  (when reactor (reactor) #_"teardown")
  (set! reactor nil))

(defn browser-main! "hot switch reactor entrypoint from CLJS REPL" [photon-main-sym]
  ;(println ::received-reload-command photon-main-sym (type photon-main-sym))
  (set! user-photon-main photon-main-sym) (stop!) (start! nil))



; user.clj

(ns user ; Must be ".clj" file, Clojure will not auto-run user.cljc
  "Start a REPL with `clj -A:dev`, or jack in with :dev alias."
  (:refer-clojure :exclude [compile])

  ; For rapid REPL startup, put absolute minimum of requires here: REPL conveniences only,
  ; which includes clojure reader extensions listed in data_readers.cljc.
  (:require [missionary.core :as m]
            hyperfiddle.rcf))

; Userland photon application code will be lazy loaded by the shadow build `(main)`
; due to :require-macros in all Photon source files.
; WARNING: make sure your REPL and shadow-cljs are sharing the same JVM!

(comment
  "Photon Clojure REPL entrypoint"
  (main)

  "turn on RCF (after all transitive dependencies have loaded)"
  (hyperfiddle.rcf/enable!)

  (shadow.cljs.devtools.api/compile :editor)

  "Switch entrypoints"
  (user/browser-main! `user-main/Main) ; see user.cljs

  "ClojureScript REPL entrypoint"
  ; shadow server exports an repl, connect a second REPL instance to it (DO NOT REUSE JVM REPL it will fail weirdly)
  (shadow.cljs.devtools.api/repl :dev)
  (type 1))

(defmacro get-main [default]
  (list 'quote (or (some-> (System/getenv "HF_DEMO") symbol) default)))

(def cljs-eval (delay @(requiring-resolve 'shadow.cljs.devtools.api/cljs-eval)))
(def shadow-start! (delay @(requiring-resolve 'shadow.cljs.devtools.server/start!)))
(def shadow-watch (delay @(requiring-resolve 'shadow.cljs.devtools.api/watch)))
(def shadow-compile (delay @(requiring-resolve 'shadow.cljs.devtools.api/compile)))
(def shadow-release (delay @(requiring-resolve 'shadow.cljs.devtools.api/release)))
(def start-server! (delay @(requiring-resolve 'hyperfiddle.photon-jetty-server/start-server!)))
(def rcf-enable! (delay @(requiring-resolve 'hyperfiddle.rcf/enable!)))

(defn browser-main! "hot switch reactor entrypoint from CLJ REPL" [photon-main-sym]
  ; Save the user the trouble of getting a CLJS repl to switch photon entrypoints
  (@cljs-eval :dev (str `(println ::loading (quote ~photon-main-sym))) {})
  (@cljs-eval :dev (str `(browser-main! (quote ~photon-main-sym))) {})
  (fn dispose [] (@cljs-eval :dev `(user/stop!) {})))

(comment (@(requiring-resolve 'shadow.cljs.devtools.api/cljs-eval) :dev (str "(println ::x)") {}))

(defn serve! "Start Photon app server" []
  (let [host "0.0.0.0"]
    (def server (@start-server! {:host host, :port 8080, :resources-path "resources/public"}))
    (println (str "\n👉 App server available at http://" host ":" (-> server (.getConnectors) first (.getPort))
                  "\n"))))

(defn rcf-shadow-hook {:shadow.build/stages #{:compile-prepare :compile-finish}} [build-state & args] build-state)

(defn install-shadow-hook! []
  (alter-var-root #'rcf-shadow-hook
                  (constantly (fn [build-state & args]
                                ;; NOTE this won’t prevent RCF tests to run during :require-macros phase
                                (case (:shadow.build/stage build-state)
                                  :compile-prepare (@rcf-enable! false)
                                  :compile-finish (@rcf-enable!))
                                build-state))))

(defn main "CLJ main" [& args]
  (println "Starting Photon compiler and server...")
  "build and serve clojurescript assets"
  (@shadow-start!)                                          ; serves index.html as well
  (@rcf-enable! false) ; don't run cljs tests on compile - in case user enabled at the REPL
  (@shadow-watch :dev) ; depends on shadow server
  ; todo report clearly if shadow build failed, i.e. due to yarn not being run
  (serve!)
  (comment (.stop server))

  "Datomic Cloud (requires :scratch alias)"
  (require '[contrib.datomic-m :as d])
  (when (not-empty (eval '(d/detect-datomic-products)))
    #_(contrib.datomic-m/install-datomic-onprem)
    (eval '(contrib.datomic-m/install-datomic-cloud))
    (def datomic-config {:server-type :dev-local :system "datomic-samples"})
    ; install prod globals
    (def datomic-client (eval '(d/client datomic-config)))
    (def datomic-conn (m/? (eval '(d/connect datomic-client {:db-name "mbrainz-subset"}))))

    ; install test globals, which can be different
    (require 'test)
    (eval '(test/install-test-state)))

  ; enable RCF after Datomic is loaded – to resolve circular dependency
  (install-shadow-hook!)
  (@rcf-enable!))

(defn compile []
  ; optimized artifact but with debug information available to find problems
  (@shadow-compile :dev))

(defn release []
  ; optimized artifact but with debug information available to find problems
  (@shadow-release :app {:debug true}))

(defn boot! []
  (compile)
  (serve!))

(when (get (System/getenv) "REPLIT_ENVIRONMENT")
  (boot!))

(when (contains? (System/getenv) "HYPERFIDDLE_DEV")
  "auto boot"
  (main)) ; this blocks the repl until build is ready. alternatively can run in a future?

