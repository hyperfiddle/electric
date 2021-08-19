(ns user
  #_(:require #_[repl :refer [refresh! refresh-all!]]
            #_[dustin.dev])
  (:require [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

#?(:clj  (alter-var-root #'hyperfiddle.rcf/*enabled* (constantly true))
   :cljs (set! hyperfiddle.rcf/*enabled* true))

; % yarn
; % yarn run css:build
; % yarn run css:watch
; % yarn run dev
; start shadow watch in shadow web control plane
; connect first nrepl for JVM repl
; load this file in clj nrepl
; start server (in the shadow JVM)
; connect second nrepl for JS repl

(comment
  ; JS
  (type 1)

  )

(comment
  ; JVM
  (do
    (require '[dev])
    (require '[hyperfiddle.api :as h])
    (require '[hyperfiddle.todomvc :as t])
    (require '[hyperfiddle.server :refer [start-server!]])
    (require '[io.pedestal.http :as http])
    (require '[shadow.cljs.devtools.api :as shadow])
    (def server (start-server! {:host   "localhost"
                                :port   8080
                                :scheme "http"})))
  (http/stop server)
  (shadow/compile :app)

  ; load the effects
  ;(require 'dustin.fiddle-pages)

  ; http://localhost:8080/dustin.fiddle-pages!page-submissions/''
  ; var foo = hyperfiddle.client.ui.demo_dataflow.main(console.log, console.error)

  )