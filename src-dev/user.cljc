(ns user
  "Photon app server build and run instructions (Clojure and ClojureScript)"
  (:require hyperfiddle.rcf))

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

  ; Optional CLJS REPL:
  ; need to export a nrepl port, can use `npx shadow-cljs -A:test server` if convenient, todo how to do this from JVM repl
  ;   connect second nrepl, then eval (shadow/repl :app) for JS repl
  ;   check repl type: eval (type 1)

  "start Photon app server"
  (do
    (require 'dev)                                          ; todo move into userland with #?(:clj (def db @(requiring-resolve 'dev/db)))
    (require '[hyperfiddle.photon :as p])
    (def server (p/start-websocket-server! {:host "localhost" :port 8080}))
    ; Wait to enable RCF after everything is loaded for fastest startup
    (hyperfiddle.rcf/enable!))

  ; http://localhost:8080/
  ; hard refresh

  "stop Photon app server"
  (.stop server)
  ; use logger when debugging due to concurrency which will interleave printlns
  #_(triage.logger/set-level! :debug)
  )

(comment
  (require 'hyperfiddle.rcf)
  #?(:clj (alter-var-root #'hyperfiddle.rcf/*generate-tests* (constantly false)))
  (hyperfiddle.rcf/enable!)
  (require 'clojure.test)
  (clojure.test/run-all-tests #"(hyperfiddle.api|user.orders)")
  )

;; Perfs
(comment
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-files 8082)
  ;; Navigate to http://localhost:8082
  (prof/start {:framebuf 10000000})
  (prof/stop)
  )

(require '[hyperfiddle.rcf.analyzer :as ana])

;; Tell RCF not to macroexpand p/run. RCF rewrites clojure, p/run is Photon.
(defmethod ana/macroexpand-hook `hyperfiddle.photon/run [_the-var _form _env args] (reduced `(hyperfiddle.photon/run ~@args)))
(defmethod ana/macroexpand-hook `hyperfiddle.photon/run2 [_the-var _form _env args] (reduced `(hyperfiddle.photon/run2 ~@args)))
(defmethod ana/macroexpand-hook `hyperfiddle.hfql/hfql [_the-var _form _env args] `(hyperfiddle.hfql/hfql ~@args))

;; Don't expand `clojure.core/binding`, photon has a special case for it.
(defmethod ana/macroexpand-hook `binding [_the-var _form _env [bindings & body]]
  `(binding ~bindings (do ~@body)))
