(ns hyperfiddle.photon-jetty-adapter
  (:require [ring.adapter.jetty9 :as jetty]
            [hyperfiddle.api :as hf]
            [hyperfiddle.logger :as log]
            [hyperfiddle.photon :as p]
            [missionary.core :as m]
            [hyperfiddle.photon-impl.io :as io]
            [hyperfiddle.photon-impl.runtime :as r])
  (:import [missionary Cancelled]
           [java.nio ByteBuffer]
           [org.eclipse.jetty.websocket.api Session SuspendToken WebSocketAdapter]))

(defn make-heartbeat
  "Ping the client to prevent connection timeout and detect unexpected disconnects.
  Close the connection when the client is not reachable anymore."
  [^Session s pong-mailbox]
  ;; Ping wisely https://developer.android.com/training/connectivity/network-access-optimization#RadioStateMachine
  (m/sp (loop []
          (m/? (m/sleep 15000))
          (.sendPing (.getRemote s) (ByteBuffer/allocate 0))
          (let [pong (m/? (m/timeout pong-mailbox 20000 :timeout))]
            (if (= :timeout pong)
              (.disconnect s)
              (recur))))))

(defn session-suspend!
  "For backpressure. Suspending the jetty ws session ensures a client cannot
  send faster than the server can process. The session is meant to be suspended
  when a message arrives and resumed once it has been processed."
  [^Session session]
  (.suspend session))

(defn token-resume!
  "Resume a websocket session so it can accept the next message from client."
  [^SuspendToken token]
  (.resume token))

(defn success [exit-value] (log/debug "Websocket handler completed gracefully." {:exit-value exit-value}))
(defn failure [^WebSocketAdapter ws ^Throwable e]
  (log/error "Websocket handler failure" e)
  ;; jetty/close! is missing arity 3 for jetty 9. Call close directly to get arity 3.
  (when-some [s (.getSession ws)] (.close s 1011 "Server process crash")))

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

(defn check-cancelled [cancel task]
  (let [t (m/race cancel task)]
    (m/sp (if-some [x (m/? t)]
            x (throw (Cancelled.))))))

(defn photon-ws-adapter
  "Start and manage a photon server process hooked onto a websocket."
  [handler-f]
  (let [cancel       (m/dfv)
        state        (atom {:session   nil ; jetty session object
                            :token     nil ; a session suspend token, used to resume a jetty session
                            :heartbeat nil ; a function cancelling the heartbeat process
                            })
        messages     (m/rdv)            ; messages from clients are put on this
                                        ; rendez-vous one by one, the photon
                                        ; process takes one, allowing the
                                        ; rendez-vous to accept the next
                                        ; message.
        pong-mailbox (m/mbx)
        resume!      (fn [_] (token-resume! (:token @state)))]
    {:on-connect (fn on-connect [^WebSocketAdapter ws]
                   (log/debug "WS connect" (jetty/req-of ws))
                   (let [session (.getSession ws)]
                     (.setMaxTextMessageSize (.getPolicy session) (* 100 1024 1024))  ; Allow large value paylods, temporary.
                     ((handler-f
                        (fn [x]
                          (check-cancelled cancel
                            (write-msg ws x)))
                        (check-cancelled cancel messages))
                      success (partial failure ws))  ; Start photon process
                     (swap! state assoc
                       :session session
                       :heartbeat ((make-heartbeat session pong-mailbox) (fn [_]) (fn [_])))))
     :on-close   (fn on-close [ws status-code reason]
                   (let [status {:status status-code, :reason reason}]
                     (case (long status-code) ; https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4.1
                       1000 (log/debug "Client disconnected gracefully" status)
                       1001 (log/debug "Client navigated away" status)
                       ;; 1005 is the default close code set by Chrome an FF unless specified.
                       1005 (log/debug "Client disconnected for an unknown reason (browser default close code)" status)
                       (log/debug "Client disconnected for an unexpected reason." status))
                     ((:heartbeat @state)) ; cancel heartbeat
                     (cancel nil)))
     :on-error   (fn on-error [ws err]
                   (log/error "Websocket error" err))
     :on-ping    (fn on-ping [ws bytebuffer]) ; Ignore client ping, no use case.
     :on-pong    (fn on-pong [ws bytebuffer]
                   (log/trace "pong")
                   (pong-mailbox bytebuffer))
     :on-text    (fn on-text [^WebSocketAdapter ws text]
                   (log/trace "text received" text)
                   ;; suspend session to backpressure client
                   (swap! state assoc :token (session-suspend! (.getSession ws)))
                   ;; deliver message to the rendez-vous, resume session when
                   ;; message is consumed by photon process.
                   ((messages text) resume! resume!))
     :on-bytes   (fn [^WebSocketAdapter ws ^bytes bytes offset length]
                   (log/trace "bytes received" {:length length})
                   (swap! state assoc :token (session-suspend! (.getSession ws)))
                   ((messages (ByteBuffer/wrap bytes offset length)) resume! resume!))}))

(defn photon-ws-message-handler
  "Given a websocket instance and a missionary task reading a message, run a photon
  program named by the client. Original HTTP upgrade ring request map is
  accessible using `(ring.adapter.jetty9/req-of ws)`."
  [ring-req write-msg read-msg]
  (binding [hf/*http-request* ring-req]
    ; Photon can resolve any dynamic var bound at this point
    (let [resolvef (bound-fn [not-found x] (r/dynamic-resolve not-found x))]
      (m/sp
        (try
          (m/? ((p/eval resolvef (io/decode (m/? read-msg)))            ; read and eval photon program sent by client
                (io/message-writer write-msg)
                (io/message-reader read-msg)))
          (catch Cancelled _))))))
