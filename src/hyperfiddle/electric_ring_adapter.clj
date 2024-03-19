(ns hyperfiddle.electric-ring-adapter
  "Provide a `wrap-electric-websocket` Ring middleware, starting and managing an Electric Server.
  This is a Ring 1.11+ compliant, generic implementation. It is compatible with
  ring-jetty out of the box, and can be extended to other servers. See
  `hyperfiddle.electric-httpkit-adapter` for an example of an extension."
  (:refer-clojure :exclude [send])
  (:require [clojure.tools.logging :as log]
            [hyperfiddle.electric :as-alias e]
            [hyperfiddle.electric.impl.io :as io]
            [hyperfiddle.electric.impl.runtime :as r]
            [hyperfiddle.electric.debug :as dbg]
            [missionary.core :as m]
            [ring.websocket :as ws])
  (:import missionary.Cancelled))

(def ELECTRIC-CONNECTION-TIMEOUT
  "Time after which the server will close the socket if it hasn't seen any websocket activity from the client."
  ;; https://www.notion.so/hyperfiddle/electric-server-heartbeat-issues-4243f981954c419f8eb0785e8e789fb7?pvs=4
  59000)

(def ELECTRIC-HEARTBEAT-INTERVAL
  "Delay between two server-send ping-emulating messages. Used to keep the connection up."
  45000)

(defprotocol Socket
  "An abstraction over various Socket impl. E.g. Ring-websocket Socket or HTTPKit
  AsyncChannel"
  (open? [this])
  (close [this code] [this code reason])
  (send [this value] [this value success-cb failure-cb]))

(defprotocol Pingable
  (ping [this] [this value])
  (pong [this] [this value]))

(defrecord RingSocket [socket]
  Socket
  (open? [_] (ws/open? socket))
  (close [_this code] (ws/close socket code ""))
  (close [_this code reason] (ws/close socket code reason))
  (send [_this value] (ws/send socket value))
  (send [_this value success-cb failure-cb] (ws/send socket value success-cb failure-cb))
  Pingable
  (ping [_this] (ws/ping socket))
  (ping [_this value] (ws/ping socket (if (string? value) (java.nio.ByteBuffer/wrap (.getBytes value)) value)))
  (pong [_this] (ws/pong socket))
  (pong [_this value] (ws/pong socket (if (string? value) (java.nio.ByteBuffer/wrap (.getBytes value)) value))))

(defn reject-websocket-handler
  "Will accept socket connection upgrade and immediately close the socket on
  connection, with given `code` and `reason`. Use this to cleanly reject a
  websocket connection."
  ;; Rejecting the HTTP 101 Upgrade request would also prevent the socket to
  ;; open, but for security reasons, the client is never informed of the HTTP
  ;; 101 failure cause.
  [code reason]
  {:on-open (fn [socket] (close (RingSocket. socket) code reason))})

(defn failure
  "Called on reactor termination, connection timeout, or reactor crash. A
  connection timeout or reactor crash will close the socket. "
  [socket ^Throwable e]
  (if (instance? Cancelled e)
    (log/debug "Websocket handler completed gracefully.")
    ;; Reactor shuts down asynchronously on socket close. User code can throw
    ;; during cancellation phase, so the reactor can fail while shutting down.
    ;; In which case socket will already be closed.
    (when (open? socket)
      (let [{::keys [type time-seconds] :as ex-data} (ex-data e)]
        (case (or type (::e/type ex-data))
          ::timeout
          (do (log/info (format "Connection to client lost after %ss. Closing socket." time-seconds))
              (close socket 1013 "Try again later"))
          ::e/misaligned-dag
          (do (log/error (ex-message e))
              (close socket 1012 "Misaligned client"))
          (do
            (log/error (dbg/update-stack-trace! e #(filter (partial dbg/stack-element-matches? #"hyperfiddle.*") %))
              "Websocket handler failure." ex-data)
            (close socket 1011 "Server process crash")))))))

(defn write-msg
  "Return a task, writing a message on a websocket when run."
  [socket message]
  (fn [s f]
    (try
      ;; Usually throws IOException, but can also throw NPE when socket remote went away.
      (send socket message (fn write-success [] (s :ack)) (fn write-failed [err] (f err)))
      (catch Throwable e (f e)))
    #()))

(defn timeout
  "Throw if `mailbox` haven't got any message after given `time` ms"
  [mailbox time]
  (m/sp
    (loop []
      (when (= :timeout (m/? (m/timeout mailbox time :timeout)))
        (throw (ex-info "No message received after specified time" {::type ::timeout, ::time-seconds (int (/ time 1000))})))
      (recur))))

(defn send-hf-heartbeat [delay ping!]
  (m/sp (loop [] (m/? (m/sleep delay)) (ping!) (recur))))

(defn- boot! [entrypoint ring-req write-msg read-msg]
  ((entrypoint ring-req) (comp write-msg io/encode) (fn [cb] (read-msg (comp cb io/decode)))))

(defmulti handle-close-status-code
  "Perform an action on socket close, dispatching on status code. List of status
  code and their meaning:
  https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4.1"
  (fn [_ring-req _socket status-code & [_reason]] status-code))

(defmethod handle-close-status-code 1000 ; normal closure
  [_ring-req _socket status-code & [reason]]
  (log/debug "Client disconnected gracefully" {:status status-code, :reason reason}))

(defmethod handle-close-status-code 1001 ; remote (client) is going away
  ;; Graceful disconnect. Typical of a hard navigation or tab close.
  [_ring-req _socket status-code & [reason]]
  (log/debug "Client navigated away" {:status status-code, :reason reason}))

(defmethod handle-close-status-code 1005 ; placeholder for no known status code
  ;; default code set by Chrome and FF unless specified.
  [_ring-req _socket status-code & [reason]]
  (log/debug "Client disconnected for an unknown reason (browser default close code)" {:status status-code, :reason reason}))

(def GENERIC-WS-CLOSE-MESSAGES
  "https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4.1"
  {1000 "Normal close"
   1001 "Client navigated away gracefully"
   1002 "Client closed websocket due to protocol error"
   1003 "Client closed websocket because it received unexpected data type"
   1004 "Websocket got closed for an unknown reason with a reserved status code."
   1005 "Client closed websocket without providing a close status code"
   1006 "Client closed websocket abnormally"
   1007 "Client closed websocket because it received a message with inconsistent data in the message (e.g. wrong encoding)"
   1008 "Client closed websocket because it received a message violating its policy."
   1009 "Client closed websocket because it received a message that is too big to be processed."
   1010 "Client closed websocket because the server failed to negotiate a client-required extension during handshake."
   1011 "Server closed websocket because of an unexpected condition."
   1015 "TLS handshake failure while establishing websocket connection."})

(defmethod handle-close-status-code :default
  [_ring-req _socket status-code & [reason]]
  (log/debug (GENERIC-WS-CLOSE-MESSAGES status-code "Client disconnected for an unexpected reason") {:status status-code :reason reason}))

(defn electric-ws-handler
  "Return a map of generic ring-compliant handlers, describing how to start and manage an Electric server process hooked onto a websocket.
  Extensions (e.g. `hyperfiddle.electric-httpkit-adapter`) can extend the handler map as needed."
  [ring-req boot-fn]
  (let [state             (object-array 2)
        on-message-slot   (int 0)
        on-close-slot     (int 1)
        keepalive-mailbox (m/mbx)]
    {:on-open    (fn on-open [socket]
                   (log/debug "WS connect" ring-req)
                   (aset state on-close-slot
                     ((m/join (fn [& _])
                        (timeout keepalive-mailbox ELECTRIC-CONNECTION-TIMEOUT)
                        (boot! boot-fn ring-req (partial write-msg socket) (r/subject-at state on-message-slot))
                        (send-hf-heartbeat ELECTRIC-HEARTBEAT-INTERVAL #(ping socket "HEARTBEAT")))
                      {} (partial failure socket)))) ; Start Electric process
     :on-close   (fn on-close [_socket _status-code & [_reason]]
                   ((aget state on-close-slot)))
     :on-error   (fn on-error [_socket err]
                   (if (and (instance? java.nio.channels.ClosedChannelException err) (nil? (ex-message err)))
                     (log/debug "Websocket was closed unexpectedly") ; common in dev
                     (log/error err "Websocket error")))
     :on-ping    (fn on-ping [socket data] ; keep connection alive
                   (keepalive-mailbox nil))
     :on-pong    (fn on-pong [_socket _bytebuffer] ; keep connection alive
                   (keepalive-mailbox nil))
     :on-message (fn on-message [_socket text-or-buff]
                   (keepalive-mailbox nil)
                   (if (instance? CharSequence text-or-buff)
                     (let [text text-or-buff]
                       (log/trace "text received" text)
                       (when-not (= "HEARTBEAT" text)
                         ((aget state on-message-slot) text)))
                     (let [^java.nio.ByteBuffer buff text-or-buff]
                       (log/trace "bytes received" (- (.limit buff) (.position buff)))
                       ((aget state on-message-slot) text-or-buff))))}))

(defn ring-ws-handler
  "Return a Ring 1.11+ websocket listener starting and managing an Electric Server process."
  [ring-req boot-fn]
  (let [{:keys [on-open on-close on-error on-ping on-pong on-message]} (electric-ws-handler ring-req boot-fn)]
    {::ws/listener
     (-> {:on-open    on-open
          :on-close   (fn [socket status-code reason]
                        (handle-close-status-code ring-req socket (long status-code) reason)
                        (on-close socket status-code reason))
          :on-error   on-error
          :on-ping    (fn [socket data]
                        (on-ping socket data)
                        (pong socket data))
          :on-pong    on-pong
          :on-message on-message}
       (update-vals
         (fn [f]
           (fn [socket & args]
             (apply f (RingSocket. socket) args)))))}))

(defn wrap-electric-websocket
  "A ring middleware starting an Electric server program defined by `electric-boot-fn` on websocket connection.
  E.g.:
  ```
  (-> ring-handler
      (wrap-electric-websocket (fn [ring-req] (e/boot-server {} my-ns/MyElectricDefn ring-req)))
      (wrap-cookies)
      (wrap-params)
      ...
    )
  ```"
  [next-handler entrypoint]
  (fn [ring-request]
    (if (ws/upgrade-request? ring-request)
      (ring-ws-handler ring-request entrypoint)
      (next-handler ring-request))))

(defn wrap-reject-stale-client
  "A Ring 1.11+ compatible middleware intercepting websocket UPGRADE request and
  checking if Electric client and Electric server versions matches.
  An Electric client is allowed to connect if:
  - its version matches the server's version,
  - the server does not have a defined version (dev mode).
  Otherwise, the websocket connection is gracefully rejected and the client is
  instructed to reload the page so to get new javascript assets.

  The rejection action can be redefined by providing an `on-mismatch` callback
  argument taking:
  - ring upgrade request,
  - client-version,
  - server-version,
  and returning the ring handler to be applied.

  e.g.
  With ring-jetty 1.11+
  ```
  (wrap-reject-stale-client handler {:hyperfiddle.electric/user-version nil})     ; will accept any client
  (wrap-reject-stale-client handler {:hyperfiddle.electric/user-version \"12345\"}) ; will only accept clients of version 12345
  ```

  With http-kit, which is not fully ring 1.11+ compliant as of Jan 9 2024
  ```
  (wrap-reject-stale-client handler {:hyperfiddle.electric/user-version \"12345\"}
    (fn on-mismatch [ring-request client-version server-version]
      (log/info 'wrap-reject-stale-client \": Electric client connection was rejected because client version doesn't match the server version. Client was instructed to perform a page reload so to get new javascript assets.\"
        {:client-version (pr-str client-version)
         :server-version (pr-str server-version)})
      (httpkit/as-channel ring-request ; this is HTTPkit specific
        (electric-httpkit/reject-websocket-handler 1008 \"stale client\") ; Websocket close code 1008 instructs the Electric client of the version mismatch
      )))
  ```"
  ([next-handler config]
   (wrap-reject-stale-client next-handler config
     (fn on-mismatch [_ring-request client-version server-version]
       (log/info 'wrap-reject-stale-client ": Electric client connection was rejected because client version doesn't match the server version. Client was instructed to perform a page reload so to get new javascript assets."
         {:client-version (pr-str client-version)
          :server-version (pr-str server-version)})
       {::ws/listener (reject-websocket-handler 1008 "stale client")}))) ; https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1
  ([next-handler {:keys [:hyperfiddle.electric/user-version]} on-missmatch]
   (fn [ring-request]
     (if (ws/upgrade-request? ring-request)
       (let [client-version (get-in ring-request [:query-params "ELECTRIC_USER_VERSION"])]
         (cond
           (nil? user-version)             (next-handler ring-request)
           (= client-version user-version) (next-handler ring-request)
           :else                           (on-missmatch ring-request client-version user-version)))
       (next-handler ring-request)))))
