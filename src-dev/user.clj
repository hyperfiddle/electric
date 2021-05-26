(ns user
  #_(:require #_[repl :refer [refresh! refresh-all!]]
            #_[dustin.dev])
  (:require [hyperfiddle.rcf :as rcf]))

(comment
  (binding [hyperfiddle.rcf/*enabled* false]
    (require '[hyperfiddle.server :refer [start-server!]])
    (require '[io.pedestal.http :as http]))

  (http/stop server)
  (def server (start-server! {:host   "localhost"
                              :port   8080
                              :scheme "http"}))
  ; load the effects
  ;(require 'dustin.fiddle-pages)

  ; http://localhost:8080/dustin.fiddle-pages!page-submissions/''


  )
