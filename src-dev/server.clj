(ns server
  (:require [ring.middleware.file :refer [wrap-file]]
            [ring.adapter.jetty9 :as ring]
            [hyperfiddle.photon-jetty-adapter :as adapter])
  (:import [java.io IOException]
           [java.net BindException]
           ))


(defn wrap-photon [next-handler]
  (fn [ring-request]
    (if (ring/ws-upgrade-request? ring-request)
      (ring/ws-upgrade-response (adapter/photon-ws-adapter adapter/photon-ws-message-handler))
      (next-handler ring-request))))

(defn default-handler [ring-request]
  {:status 404
   :body "not found"})

(defn start-server! [config]
  (try
    (ring/run-jetty (-> default-handler (wrap-file "resources/public") (wrap-photon))
      (merge {:port 8080, :join? false} config))
    (catch IOException err
      (if (instance? BindException (ex-cause err))
        (do (hyperfiddle.logger/warn "Port" (:port config) "was not available, retrying with" (inc (:port config)))
          (start-server! (update config :port inc)))
        (throw err)))))

