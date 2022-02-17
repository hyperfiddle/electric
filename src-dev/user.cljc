(ns user )

; % npx shadow-cljs server
; connect one nrepl for JVM repl
; connect second nrepl, then eval (shadow/repl :app) for JS repl
; check repl type: eval (type 1)

;;;;;;;;;;;;;;;;;;
;; Start server ;;
;;;;;;;;;;;;;;;;;;
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
  )

;;;;;;;;;;;;;;;;;;;;
;; Compile client ;;
;;;;;;;;;;;;;;;;;;;;
(comment
  (shadow/compile :app)
  (shadow/release :app)
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
  )
