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

(defn make-heartbeat
  "Ping the client to prevent connection timeout and detect unexpected disconnects.
  Close the connection when the client is not reachable anymore."
  [^Session s pong-mailbox]
  ;; Ping wisely https://developer.android.com/training/connectivity/network-access-optimization#RadioStateMachine
  (m/sp
    (loop []
      (m/? (m/sleep 15000))
      (.sendPing (.getRemote s) (ByteBuffer/allocate 0))
      (when-not (m/? (m/timeout pong-mailbox 20000))
        (throw (ex-info "Websocket pong timeout." {})))
      (recur))))

(defn failure [^WebSocketAdapter ws ^Throwable e]
  (if (instance? Cancelled e)
    (log/debug "Websocket handler completed gracefully.")
    (do (log/error e "Websocket handler failure")
        ;; jetty/close! is missing arity 3 for jetty 9. Call close directly to get arity 3.
        (when-some [s (.getSession ws)] (.close s 1011 "Server process crash")))))

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

(defn electric-ws-adapter
  "Start and manage an Electric server process hooked onto a websocket."
  [handler-f]
  (let [state        (object-array 2)
        on-message-slot (int 0)
        on-close-slot   (int 1)
        pong-mailbox (m/mbx)]
    {:on-connect (fn on-connect [^WebSocketAdapter ws]
                   (log/debug "WS connect" (jetty/req-of ws))
                   (let [session (.getSession ws)]
                     (.setMaxTextMessageSize (.getPolicy session) (* 100 1024 1024))  ; Allow large value paylods, temporary.
                     (aset state on-close-slot
                       ((m/join {} (make-heartbeat session pong-mailbox)
                          (handler-f (partial write-msg ws)
                            (r/subject-at state on-message-slot)))
                        {} (partial failure ws)))))  ; Start Electric process
     :on-close   (fn on-close [ws status-code reason]
                   (let [status {:status status-code, :reason reason}]
                     (case (long status-code) ; https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4.1
                       1000 (log/debug "Client disconnected gracefully" status)
                       1001 (log/debug "Client navigated away" status)
                       ;; 1005 is the default close code set by Chrome an FF unless specified.
                       1005 (log/debug "Client disconnected for an unknown reason (browser default close code)" status)
                       (log/debug "Client disconnected for an unexpected reason." status))
                     ((aget state on-close-slot))))
     :on-error   (fn on-error [ws err]
                   (log/error err "Websocket error"))
     :on-ping    (fn on-ping [ws bytebuffer]) ; Ignore client ping, no use case.
     :on-pong    (fn on-pong [ws bytebuffer]
                   (log/trace "pong")
                   (pong-mailbox bytebuffer))
     :on-text    (fn on-text [^WebSocketAdapter ws text]
                   (log/trace "text received" text)
                   ((aget state on-message-slot) text))
     :on-bytes   (fn [^WebSocketAdapter ws ^bytes bytes offset length]
                   (log/trace "bytes received" {:length length})
                   ((aget state on-message-slot) (ByteBuffer/wrap bytes offset length)))}))

(defn electric-ws-message-handler
  "Given a ring request, a writer task function and a subject emitting messages, run an Electric
  program named by the client. Original HTTP upgrade ring request map is
  accessible using `(ring.adapter.jetty9/req-of ws)`."
  [ring-req write-msg read-msg]
  (binding [e/*http-request* ring-req]
    ;; Electric can resolve any dynamic var bound at this point
    (let [resolvef (bound-fn [not-found x] (r/dynamic-resolve not-found x))]
      (m/sp
        (m/? ((e/eval resolvef (io/decode (m/? (m/reduce (comp reduced {}) nil (m/observe read-msg)))))   ; read and eval Electric program sent by client
              (comp write-msg io/encode) (fn [cb] (read-msg (comp cb io/decode)))))))))

(defn reject-websocket-handler
  "Will accept socket connection upgrade and immediately close the socket on
  connection, with given `code` and `reason`. Use this to cleanly reject a
  websocket connection."
  ;; Rejecting the HTTP 101 Upgrade request would also prevent the socket to
  ;; open, but for security reasons, the client is never informed of the HTTP
  ;; 101 failure cause.
  [code reason]
  {:on-connect (fn [^WebSocketAdapter ws] (.. ws getSession (close code reason)))})
