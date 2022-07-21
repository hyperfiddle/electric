(ns hyperfiddle.photon-jetty-adapter
  (:require [ring.adapter.jetty9 :as jetty]
            [hyperfiddle.logger :as log]
            [hyperfiddle.photon :as p]
            [missionary.core :as m]
            [hyperfiddle.photon-impl.io :as io])
  (:import [missionary Cancelled]
           [java.nio ByteBuffer]
           [org.eclipse.jetty.websocket.api Session SuspendToken]))

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

(defn photon-ws-adapter
  "Start and manage a photon server process hooked onto a websocket."
  [photon-ws-message-handler]
  (let [state        (atom {:session   nil ; jetty session object
                            :cancel!   nil ; a function cancelling the current photon process
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
    {:on-connect (fn on-connect [ws]
                   (log/debug "WS connect" (jetty/req-of ws))
                   (let [session (.getSession ws)]
                     (swap! state assoc
                       :session   session
                       :cancel!   (photon-ws-message-handler ws messages)  ; Start photon process
                       :heartbeat ((make-heartbeat session pong-mailbox) (fn [_]) (fn [_])))))
     :on-close   (fn on-close [ws status-code reason]
                   (let [status  {:status status-code, :reason reason}]
                     (case status-code ; https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4.1
                       1000 (log/debug "Client disconnected gracefully" status)
                       1001 (log/debug "Client navigated away" status)
                       ;; 1005 is the default close code set by Chrome an FF unless specified.
                       1005 (log/info  "Client disconnected for an unknown reason (browser default close code)" status)
                       (log/error "Client socket disconnected for an unexpected reason." status))
                     ((:heartbeat @state)) ; cancel heartbeat
                     ((:cancel! @state))   ; cancel (terminate) photon process
                     ))
     :on-error   (fn on-error [ws err]
                   (log/error "Websocket error" err))
     :on-ping    (fn on-ping [ws bytebuffer]) ; Ignore client ping, no use case.
     :on-pong    (fn on-pong [ws bytebuffer]
                   (log/trace "pong")
                   (pong-mailbox bytebuffer))
     :on-text    (fn on-text [ws text]
                   (log/trace "text received" text)
                   ;; suspend session to backpressure client
                   (swap! state assoc :token (session-suspend! (.getSession ws)))
                   ;; deliver message to the rendez-vous, resume session when
                   ;; message is consumed by photon process.
                   ((messages text) resume! resume!))
     :on-bytes   (fn [ws ^bytes bytes offset length]
                   (log/trace "bytes received" {:length length})
                   (swap! state assoc :token (session-suspend! (.getSession ws)))
                   ((messages (ByteBuffer/wrap bytes offset length)) resume! resume!))}))

(defn write-msg
  "Return a task, writing a message on a websocket when run."
  [ws message]
  (fn [s f]
    (jetty/send! ws message {:write-failed  (fn write-failed [err] (f err))
                             :write-success (fn write-success [] (s nil))})
    #()))

(defn success [exit-value] (log/debug "Websocket handler completed gracefully." {:exit-value exit-value}))
(defn failure [ws ^Throwable e]
  (log/error "Websocket handler failure" e)
  ;; jetty/close! is missing arity 3 for jetty 9. Call close directly to get arity 3.
  (.. ws (getSession) (close 1011 "Server process crash")))

(defn photon-ws-message-handler
  "Given a websocket instance and a missionary task reading a message, run a photon
  program named by the client. Original HTTP upgrade ring request map is
  accessible using `(ring.adapter.jetty9/req-of ws)`."
  [ws read-msg]
  ((m/sp
     (try
       (m/? ((p/eval (io/decode (m/? read-msg)))            ; read and eval photon program sent by client
             (io/message-writer (partial write-msg ws))
             (io/message-reader read-msg)))
       (catch Cancelled _)))
   success (partial failure ws)))
