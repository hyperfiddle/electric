(ns user ; Must be ".clj" file, Clojure doesn't auto-load user.cljc
  "Start a REPL with `clj -A:dev`, or jack in with :dev alias."
  (:refer-clojure :exclude [compile])
  ;; For fastest REPL startup, no heavy deps here, REPL conveniences only
  ;; (Clojure has to compile all this stuff on startup)
  (:require [missionary.core :as m]
            hyperfiddle.rcf))

;; WARNING: make sure your REPL and shadow-cljs are sharing the same JVM!

(hyperfiddle.rcf/enable! false)

(comment
  (main) ; Electric Clojure(JVM) REPL entrypoint
  (hyperfiddle.rcf/enable!) ; turn on RCF after all transitive deps have loaded
  (shadow.cljs.devtools.api/repl :dev) ; shadow server hosts the cljs repl
  ; connect a second REPL instance to it
  ; (DO NOT REUSE JVM REPL it will fail weirdly)
  (type 1))

(def shadow-start! (delay @(requiring-resolve 'shadow.cljs.devtools.server/start!)))
(def shadow-watch (delay @(requiring-resolve 'shadow.cljs.devtools.api/watch)))
(def shadow-compile (delay @(requiring-resolve 'shadow.cljs.devtools.api/compile)))
(def shadow-release (delay @(requiring-resolve 'shadow.cljs.devtools.api/release)))
(def start-electric-server! (delay @(requiring-resolve 'electric-server-java8-jetty9/start-server!)))
(def rcf-enable! (delay @(requiring-resolve 'hyperfiddle.rcf/enable!)))

(defn rcf-shadow-hook {:shadow.build/stages #{:compile-prepare :compile-finish}}
  [build-state & args] build-state)

(defn install-rcf-shadow-hook []
  (alter-var-root #'rcf-shadow-hook
                  (constantly (fn [build-state & args]
                                ;; NOTE this won’t prevent RCF tests to run during :require-macros phase
                                (case (:shadow.build/stage build-state)
                                  :compile-prepare (@rcf-enable! false)
                                  :compile-finish (@rcf-enable!))
                                build-state))))

(def electric-server-config {:host "0.0.0.0", :port 8080, :resources-path "public", :manifest-path "public/js/manifest.edn"})

(defn main [& args]
  (println "Starting Electric compiler and server...")
  (@shadow-start!) ; serves index.html as well
  (@rcf-enable! false) ; don't run cljs tests on compile (user may have enabled it at the REPL already)
  (@shadow-watch :dev) ; depends on shadow server
  #_(@shadow-release :dev {:debug false})
  ;; todo report clearly if shadow build failed, i.e. due to yarn not being run
  (def server (@start-electric-server! electric-server-config))
  (comment (.stop server))

  "Datomic Cloud (optional, requires :scratch alias)"
  ;; failing with new datomic peer dependency
  ;; resolve issues once this is needed
  #_(require '[contrib.datomic-m :as d])
  #_(when (not-empty (eval '(d/detect-datomic-products)))
    #_(contrib.datomic-m/install-datomic-onprem)
    (eval '(contrib.datomic-m/install-datomic-cloud))
    (def datomic-config {:server-type :dev-local :system "datomic-samples"})
    ;; install prod globals
    (def datomic-client (eval '(d/client datomic-config)))
    (def datomic-conn (m/? (eval '(d/connect datomic-client {:db-name "mbrainz-subset"}))))

    ;; install test globals, which are different
    (require 'test)
    (eval '(test/install-test-state)))

  ;; enable RCF after Datomic is loaded – to resolve circular dependency
  (install-rcf-shadow-hook)
  (@rcf-enable!))

;; shadow-compile vs shadow-release:
;; https://shadow-cljs.github.io/docs/UsersGuide.html#_development_mode
(defn release "closure optimized target"
  [& {:as kwargs}] (@shadow-release :dev (merge {:debug false} kwargs)))

;; (when (get (System/getenv) "REPLIT_ENVIRONMENT")
;;  (compile) (@start-electric-server! electric-server-config))

(when (= "true" (get (System/getenv) "HYPERFIDDLE_AUTO_BOOT"))
  (main))

