(ns user
  #_(:require #_[repl :refer [refresh! refresh-all!]]
            #_[dustin.dev])
  (:require [hyperfiddle.rcf :as rcf]))

(comment
  (rcf/with-config {:enabled false}
    (require '[hyperfiddle.server :refer [start-server!]])
    (require '[io.pedestal.http :as http]))

  (def server (start-server! {:host   "localhost"
                              :port   8080
                              :scheme "http"}))

  (require 'dustin.fiddle-pages)                            ; load the effects

  (http/stop server)
  )
