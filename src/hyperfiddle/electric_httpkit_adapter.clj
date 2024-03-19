(ns hyperfiddle.electric-httpkit-adapter
  "Provide a `wrap-electric-websocket` HTTPKit compatible middleware, starting and
  managing an Electric Server. This is a variant of
  `hyperfiddle.electric-ring-adapter` made compatible with HTTPKit."
  (:require
   [clojure.tools.logging :as log]
   [hyperfiddle.electric-ring-adapter :as ering]
   [org.httpkit.server :as httpkit]
   [ring.websocket :as ws])
  (:import
   (org.httpkit.server AsyncChannel)))

(defrecord HTTPKitSocket [^AsyncChannel channel]
  ering/Socket
  (open? [_] (httpkit/open? channel))
  (close [_this code] (.serverClose channel code))
  (close [_this code _reason] (.serverClose channel code)) ; HTTPKit doesn't support close reason
  (send [_this value] (httpkit/send! channel {:body value}))
  (send [_this value success-cb failure-cb]
    (if (httpkit/send! channel {:body value})
      (success-cb)
      (failure-cb (ex-info "Can't send message to client, remote channel is closed" {}))))

  ;; ping and pong are not exposed by HTTPKit. Instead ping is replaced by a
  ;; special "HEARTBEAT" message that the client will echo. HTTPKit will
  ;; automatically answer client pings with an immediate echo pong.
  ering/Pingable
  (ping [this] (ering/ping this "HEARTBEAT"))
  (ping [this value] (assert (= "HEARTBEAT" value)) (ering/send this value))
  (pong [this] (throw (ex-info "Pong is not supported" {})))
  (pong [this value] (throw (ex-info "Pong with arbitrary data is not supported" {})))
  )

(defn reject-websocket-handler
  "Will accept socket connection upgrade and immediately close the socket on
  connection, with given `code` and `reason`. Use this to cleanly reject a
  websocket connection."
  ;; Rejecting the HTTP 101 Upgrade request would also prevent the socket to
  ;; open, but for security reasons, the client is never informed of the HTTP
  ;; 101 failure cause.
  [code reason]
  {:on-open (fn [socket] (ering/close (HTTPKitSocket. socket) code reason))})

(def STATUS-CODE
  "Map HTTPKit custom WS status names to the actual RFC-defined status code, if it
  can be mapped. Fully qualify the status name otherwise."
  {:server-close          ::server-close
   :client-close          ::client-close
   :normal                1000
   :going-away            1001
   :protocol-error        1002
   :unsupported           1003
   :no-status-received    1005
   :abnormal              1006
   :invalid-payload-data  1007
   :policy-violation      1008
   :message-too-big       1009
   :mandatory-extension   1010
   :internal-server-error 1011
   :tls-handshake         1015
   :unknown               ::unknown})

(defmethod ering/handle-close-status-code ::server
  [_ring-req _socket _status-code & [_reason]]
  (log/debug "HTTPKit server closed the websocket connection"))

(defmethod ering/handle-close-status-code ::client
  [_ring-req _socket _status-code & [_reason]]
  (log/debug "Websocket client closed the connection for an unknown reason"))

(defmethod ering/handle-close-status-code ::unknown
  [_ring-req _socket _status-code & [_reason]]
  (log/debug "HTTPKit websocket connection closed for an unknown reason"))

(defn httpkit-ws-handler
  "Return a map of HTTPkit-compatible handlers, describing how to start and manage an Electric server process, hooked onto a websocket."
  [ring-req boot-fn]
  (let [{:keys [on-open on-close on-ping #_on-pong #_on-error on-message]} (ering/electric-ws-handler ring-req boot-fn)]
    (-> {:init       (fn [_socket]) ; called pre handshake, no use case
         :on-open    on-open
         :on-close   (fn [socket status-code]
                       (ering/handle-close-status-code ring-req socket (or (STATUS-CODE status-code) status-code))
                       (on-close socket status-code))
         :on-ping    on-ping
         ;; :on-pong    on-pong  ; unsupported by HTTPKit
         ;; :on-error   on-error ; unsupported by HTTPKit
         :on-receive on-message}
      (update-vals
        (fn [f]
          (fn [async-channel & args]
            (apply f (HTTPKitSocket. async-channel) args)))))))

(defn wrap-electric-websocket
  "An HTTPKit-compatible ring middleware, starting an Electric server program defined by `electric-boot-fn` on websocket connection.
  E.g.: ```
  (-> ring-handler
      (wrap-electric-websocket (fn [ring-req] (e/boot-server {} my-ns/MyElectricDefn ring-req)))
      (wrap-cookies)
      (wrap-params)
    )
  ```
  "
  [next-handler electric-boot-fn]
  (fn [ring-request]
    (if (ws/upgrade-request? ring-request)
      (httpkit/as-channel ring-request
        (httpkit-ws-handler ring-request electric-boot-fn))
      (next-handler ring-request))))
