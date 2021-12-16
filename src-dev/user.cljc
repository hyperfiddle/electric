(ns user)

; % yarn
; % yarn run css:build
; % yarn run css:watch
; % yarn run dev
; start shadow watch in shadow web control plane
; connect first nrepl for JVM repl
; load this file in clj nrepl
; start server (in the shadow JVM)
; connect second nrepl for JS repl

;; Start server and compile client
(comment
  (do
    (require '[dev])
    (require '[hyperfiddle.server :refer [start-server!]])
    (require '[io.pedestal.http :as http])
    (require '[shadow.cljs.devtools.api :as shadow])
    (def server (start-server! {:host   "localhost"
                                :port   8080
                                :scheme "http"})))
  (http/stop server)
  (shadow/compile :app)
  )

;; Check REPL type
(comment
  (type 1)
  )

;; Tests
(comment
  (require '[hyperfiddle.rcf])

  #?(:clj (alter-var-root #'hyperfiddle.rcf/*generate-tests* (constantly false)))
  #?(:clj  (alter-var-root #'hyperfiddle.rcf/*enabled* (constantly true))
     :cljs (set! hyperfiddle.rcf/*enabled* true))

  (require '[clojure.test])
  (clojure.test/run-all-tests #"(hyperfiddle.api|hyperfiddle.q5|user.gender-shirt-size)")

  ; load the effects
  ;(require 'dustin.fiddle-pages)

  ; http://localhost:8080/dustin.fiddle-pages!page-submissions/''
  ; var foo = hyperfiddle.client.ui.demo_dataflow.main(console.log, console.error)
  )

;; Perfs
(comment
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-files 8082)
  (prof/start) ; start profiler
  (prof/stop)
  )
