(ns user )

; % rm .cpcache .shadow-cljs
; % yarn
; % npm install
; % npx shadow-cljs -A:test server
; http://localhost:9630/build/app ; click watch
; nrepl to shadow JVM ; nrepl port in shadow-cljs.edn
; start photon server per below comment block
; http://localhost:8080/ ; hard refresh
;
; Optional CLJS REPL:
;   connect second nrepl, then eval (shadow/repl :app) for JS repl
;   check repl type: eval (type 1)
;
; Warning:
;   Commonly we think our macros confuse shadow. 99% of the time this is not the case.
;   the problem is sharing JVM with shadow and the JVM state is the problem.
;   Restart the JVM by restarting shadow.

;;;;;;;;;;;;;;;;;;
;; Start server ;;
;;;;;;;;;;;;;;;;;;
(comment
  (do
    (require '[dev])
    (require 'hyperfiddle.rcf)
    (require '[hyperfiddle.server :refer [start-server!]])
    (require '[io.pedestal.http :as http]
             '[shadow.cljs.devtools.api :as shadow])
    (def server (start-server! {:host   "localhost"
                                :port   8080
                                :scheme "http"}))
    (hyperfiddle.rcf/enable!))

  (http/stop server)

  #_(hyperfiddle.dev.logger/set-level! :debug)
  ; use logger when debugging due to concurrency which will interleave printlns
  )

;;;;;;;;;;;;;;;;;;;;
;; Compile client ;;
;;;;;;;;;;;;;;;;;;;;
(comment
  (shadow/compile :app)
  (shadow/release :app)

  ; Watch works
  ; Make sure to eval on JVM first, then save file to trigger cljs compile and reload
  (shadow/watch :app)

  )

;; Tests
(comment
  (require '[hyperfiddle.rcf])

  #?(:clj (alter-var-root #'hyperfiddle.rcf/*generate-tests* (constantly false)))
  #?(:clj  (alter-var-root #'hyperfiddle.rcf/*enabled* (constantly true))
     :cljs (set! hyperfiddle.rcf/*enabled* true))

  (require '[clojure.test])
  (clojure.test/run-all-tests #"(hyperfiddle.api|user.gender-shirt-size)")

  ; load the effects
  ;(require 'dustin.fiddle-pages)

  ; http://localhost:8080/dustin.fiddle-pages!page-submissions/''
  ; var foo = hyperfiddle.client.ui.demo_dataflow.main(console.log, console.error)
  )

;; Perfs
(comment
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-files 8082)
  ;; Navigate to http://localhost:8082
  (prof/start {:framebuf 10000000})
  (prof/stop)
  )
