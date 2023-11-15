(ns hyperfiddle.electric-jetty-adapter
  (:require [ring.adapter.jetty9 :as jetty]
            [clojure.tools.logging :as log]
            [hyperfiddle.electric :as e]
            [missionary.core :as m]
            [hyperfiddle.electric.impl.io :as io]
            [hyperfiddle.electric.impl.runtime :as r])
  (:import [missionary Cancelled]
           [java.nio ByteBuffer]
           [org.eclipse.jetty.websocket.api Session WebSocketAdapter]))

(defn failure [^WebSocketAdapter ws ^Throwable e]
  (if (instance? Cancelled e)
    (log/debug "Websocket handler completed gracefully.")
    ;; jetty/close! is missing arity 3 for jetty 9. Call close directly to get arity 3.
    (when-some [s (.getSession ws)]
      (let [{::keys [type time-seconds] :as data} (ex-data e)]
        (case type
          ::timeout (do (log/info (format "Connection to client lost after %ss. Closing socket." time-seconds))
                        (.close s 1013 "Try again later"))
          (do (log/error e "Websocket handler failure." data)
              (.close s 1011 "Server process crash")))))))

(defn write-msg
  "Return a task, writing a message on a websocket when run."
  [ws message]
  (fn [s f]
    (try
      (jetty/send! ws message {:write-failed  (fn write-failed [err] (f err))
                               :write-success (fn write-success [] (s :ack))})
      ;; jetty/send! throws NPE when ws remote is null
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

(defn send-hf-heartbeat [delay send!]
  (m/sp (loop [] (m/? (m/sleep delay)) (send! "HEARTBEAT") (recur))))

(def ELECTRIC-CONNECTION-TIMEOUT 59000) ; https://www.notion.so/hyperfiddle/electric-server-heartbeat-issues-4243f981954c419f8eb0785e8e789fb7?pvs=4
(def ELECTRIC-HEARTBEAT-INTERVAL 45000)

(defn electric-ws-adapter
  "Start and manage an Electric server process hooked onto a websocket."
  [handler-f]
  (let [state             (object-array 2)
        on-message-slot   (int 0)
        on-close-slot     (int 1)
        keepalive-mailbox (m/mbx)]
    {:on-connect (fn on-connect [^WebSocketAdapter ws]
                   (log/debug "WS connect" (jetty/req-of ws))
                   (.setMaxTextMessageSize (.getPolicy (.getSession ws)) (* 100 1024 1024))  ; Allow large value payloads, temporary.
                   (aset state on-close-slot
                     ((m/join (fn [& _])
                        (timeout keepalive-mailbox ELECTRIC-CONNECTION-TIMEOUT)
                        (handler-f (partial write-msg ws) (r/subject-at state on-message-slot))
                        (send-hf-heartbeat ELECTRIC-HEARTBEAT-INTERVAL #(jetty/send! ws %)))
                      {} (partial failure ws)))) ; Start Electric process
     :on-close   (fn on-close [ws status-code reason]
                   (let [status {:status status-code, :reason reason}]
                     (case (long status-code) ; https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4.1
                       1000 (log/debug "Client disconnected gracefully" status)
                       1001 (log/debug "Client navigated away" status)
                       ;; 1005 is the default close code set by Chrome and FF unless specified.
                       1005 (log/debug "Client disconnected for an unknown reason (browser default close code)" status)
                       (log/debug "Client disconnected for an unexpected reason." status))
                     ((aget state on-close-slot))))
     :on-error   (fn on-error [ws err]
                   (log/error err "Websocket error"))
     :on-ping    (fn on-ping [^WebSocketAdapter ws bytebuffer] ; Send pong and keep connection alive.
                   (.sendPong (.getRemote (.getSession ws)) bytebuffer)
                   (keepalive-mailbox nil))
     :on-pong    (fn on-pong [ws bytebuffer] ; ignore pong, no use case
                   (log/trace "pong"))
     :on-text    (fn on-text [^WebSocketAdapter ws text]
                   (log/trace "text received" text)
                   (when-not (= "HEARTBEAT" text)
                     ((aget state on-message-slot) text))
                   (keepalive-mailbox nil))
     :on-bytes   (fn [^WebSocketAdapter ws ^bytes bytes offset length]
                   (log/trace "bytes received" {:length length})
                   ((aget state on-message-slot) (ByteBuffer/wrap bytes offset length))
                   (keepalive-mailbox nil))}))

(defn electric-ws-message-handler
  "Given a ring request, a writer task function and a subject emitting messages, run an Electric
  program named by the client. Original HTTP upgrade ring request map is
  accessible using `(ring.adapter.jetty9/req-of ws)`."
  [ring-req entrypoint write-msg read-msg]
  ((entrypoint ring-req) (comp write-msg io/encode) (fn [cb] (read-msg (comp cb io/decode)))))

(defn reject-websocket-handler
  "Will accept socket connection upgrade and immediately close the socket on
  connection, with given `code` and `reason`. Use this to cleanly reject a
  websocket connection."
  ;; Rejecting the HTTP 101 Upgrade request would also prevent the socket to
  ;; open, but for security reasons, the client is never informed of the HTTP
  ;; 101 failure cause.
  [code reason]
  {:on-connect (fn [^WebSocketAdapter ws] (.. ws getSession (close code reason)))})
