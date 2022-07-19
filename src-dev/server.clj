(ns server
  (:require [ring.middleware.file :refer [wrap-file]]
            [ring.adapter.jetty9 :as ring]
            [hyperfiddle.photon-jetty-adapter :as adapter]
            [hyperfiddle.logger :as log])
  (:import [java.io IOException]
           [java.net BindException]
           ))

#_
(defn wrap-photon [next-handler]        ; Jetty 10 allows such handler
  (fn [ring-request]
    (if (ring/ws-upgrade-request? ring-request)
      (ring/ws-upgrade-response (adapter/photon-ws-adapter adapter/photon-ws-message-handler))
      (next-handler ring-request))))

(defn default-handler [ring-request]
  {:status 404
   :body "not found"})

(defn start-server! [config]
  (try
    (ring/run-jetty (-> default-handler
                      (wrap-file "resources/public")
                      #_(wrap-photon))
      ;; Jetty 9 forces us to declare WS paths out of a ring handler.
      (merge {:port       8080
              :join?      false
              :websockets {"/" (fn [ring-req] (adapter/photon-ws-adapter adapter/photon-ws-message-handler))}}
        config))
    (catch IOException err
      (if (instance? BindException (ex-cause err))
        (do (log/warn "Port" (:port config) "was not available, retrying with" (inc (:port config)))
          (start-server! (update config :port inc)))
        (throw err)))))

