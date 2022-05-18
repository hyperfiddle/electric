(ns user
  "Photon app server build and run instructions (Clojure and ClojureScript)"
  (:require clojure.string
            [hyperfiddle.rcf :refer [tests]]))

(comment
  "clean cljs state"
  ; % rm .cpcache .shadow-cljs
  ; % yarn
  ; launch JVM repl under :test deps alias
  (require 'shadow.cljs.devtools.server)
  (shadow.cljs.devtools.server/start!)

  ; Watch works. Make sure to eval on JVM first, then save file to trigger cljs compile and reload
  (require '[shadow.cljs.devtools.api :as shadow])
  (shadow/watch :app)                                       ; depends on shadow server
  #_(shadow/compile :app)
  #_(shadow/release :app)

  ; Warning:
  ;   Commonly we think our macros confuse shadow. 99% of the time this is not the case.
  ;   the problem is sharing JVM with shadow and the JVM state is the problem.
  ;   Restart the JVM by restarting shadow.

  "start Photon app server"
  (do
    (require 'dev)                                          ; todo move into userland with #?(:clj (def db @(requiring-resolve 'dev/db)))
    (require '[hyperfiddle.photon :as p])
    #_(hyperfiddle.logger/set-level! :debug)
    (def server (p/start-websocket-server! {:host "localhost" :port 8081}))
    (comment (.stop server))
    ; Wait to enable RCF after everything is loaded for fastest startup
    (hyperfiddle.rcf/enable!))

  ; http://localhost:8080/
  ; hard refresh

  "Optional CLJS REPL"
  ; shadow server exports an repl
  ; connect a secondary repl instance to this (DO NOT REUSE JVM REPL it will fail weirdly)
  ;   check repl type: eval (type 1)
  (shadow.cljs.devtools.api/repl :app)
  (hyperfiddle.rcf/enable!)                                 ; again in cljs repl
  (tests (pr-str (type 1)) := "#object[Number]")
  )

(comment
  "CI tests"
  #?(:clj (alter-var-root #'hyperfiddle.rcf/*generate-tests* (constantly false)))
  (hyperfiddle.rcf/enable!)
  (require 'clojure.test)
  (clojure.test/run-all-tests #"(hyperfiddle.api|user.orders)")
  )

(comment
  "Performance profiling"
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-files 8082)
  ;; Navigate to http://localhost:8082
  (prof/start {:framebuf 10000000})
  (prof/stop)
  )

(defn includes-str? "used repeatedly in tutorials" [v needle]
  (clojure.string/includes? (clojure.string/lower-case (str v))
                            (clojure.string/lower-case (str needle))))

(tests
  (includes-str? "alice" "e") := true
  (includes-str? "alice" "f") := false
  (includes-str? "alice" "") := true
  (includes-str? "alice" nil) := true
  (includes-str? nil nil) := true
  (includes-str? nil "") := true
  (includes-str? "" nil) := true
  )