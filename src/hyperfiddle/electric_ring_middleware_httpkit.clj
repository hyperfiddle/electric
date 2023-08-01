(ns hyperfiddle.electric-ring-middleware-httpkit
  (:require [org.httpkit.server :as http-kit]
            [hyperfiddle.electric-httpkit-adapter :as electric]))

(def ^:const VERSION (not-empty (System/getProperty "HYPERFIDDLE_ELECTRIC_SERVER_VERSION"))) ; to be set in prod

(defn wrap-electric [handler]
  (fn [req]
    (if (:websocket? req)
      (http-kit/as-channel req
        (let [client-version (get-in req [:query-params "HYPERFIDDLE_ELECTRIC_CLIENT_VERSION"])]
          (if (or (nil? VERSION) (= client-version VERSION))
            (electric/handle-electric-ws req (partial electric/electric-ws-message-handler req))
            (electric/reject-websocket-handler 1008) ; stale client - https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1
            )))
      (handler req))))
