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
           [org.eclipse.jetty.websocket.api Session WebSocketAdapter]))

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

(defn photon-ws-adapter
  "Start and manage a photon server process hooked onto a websocket."
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
                       ((m/race
                          (handler-f (partial write-msg ws)
                            (r/subject-at state on-message-slot))
                          (make-heartbeat session pong-mailbox))
                        success (partial failure ws)))))  ; Start photon process
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
                   (log/error "Websocket error" err))
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

(defn photon-ws-message-handler
  "Given a ring request, a writer task function and a subject emitting messages, run a photon
  program named by the client. Original HTTP upgrade ring request map is
  accessible using `(ring.adapter.jetty9/req-of ws)`."
  [ring-req write-msg read-msg]
  (binding [hf/*http-request* ring-req]
    ; Photon can resolve any dynamic var bound at this point
    (let [resolvef (bound-fn [not-found x] (r/dynamic-resolve not-found x))]
      (m/sp
        (try
          (m/? ((p/eval resolvef (io/decode (m/? (m/reduce (comp reduced {}) nil (m/observe read-msg)))))   ; read and eval photon program sent by client
                (partial (io/encoder (fn [r x] (m/sp (m/? r) (m/? (write-msg x))))) (m/sp))
                (comp read-msg (partial partial (io/decoder io/foreach)))))
          (catch Cancelled _))))))
