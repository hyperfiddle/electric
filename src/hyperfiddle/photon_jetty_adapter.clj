(ns hyperfiddle.photon-jetty-adapter
  (:require [ring.adapter.jetty9 :as jetty] ;; TODO bloated dependency, extract minimal feature set.
            [hyperfiddle.logger :as log]
            [hyperfiddle.photon :as p]
            [missionary.core :as m]
            [hyperfiddle.photon-impl.io :as io])
  (:import [missionary Cancelled]
           [java.nio ByteBuffer]
           [java.util.concurrent Executors ThreadFactory Executor]
           [org.eclipse.jetty.websocket.api Session SuspendToken]))

(def event-loop ; TODO remove, not necessary with a deadlock-free reactor.
  (let [procs (.availableProcessors (Runtime/getRuntime))
        execs (into []
                (map (fn [i]
                       (Executors/newSingleThreadExecutor
                         (reify ThreadFactory
                           (newThread [_ r]
                             (doto (Thread. r (str "hf-eventloop-" i))
                               (.setDaemon true)))))))
                (range procs))]
    (fn [x] (nth execs (mod (hash x) procs)))))

(defn run-via [^Executor e f] ; TODO remove, not necessary with a deadlock-free reactor.
  (fn [& args]
    (.execute e
      (reify Runnable
        (run [_]
          (apply f args))))))

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
                   (let [close!* (fn []
                                   ((:heartbeat @state)) ; cancel heartbeat
                                   ((:cancel! @state))   ; cancel (terminate) photon process
                                   )
                         status  {:status status-code, :reason reason}]
                     (case status-code ; https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent/code
                       (1000 1001) (do (log/info "Client disconnected" status)
                                       (close!*))
                       (do (log/error "Client socket disconnected for an unexpected reason." status)
                           (close!*)))))
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

(defn success [exit-value] (log/info "Websocket handler completed gracefully." exit-value))
(defn failure [^Throwable e] (log/error "Websocket handler failure" e))

(defn photon-ws-message-handler
  "Given a websocket instance and a missionary task reading a message, run a photon
  program named by the client. Original HTTP upgrade ring request map is
  accessible using `(ring.adapter.jetty9/req-of ws)`."
  [ws read-msg]
  (let [el        (event-loop ws)
        read-task (m/sp (try (m/? read-msg) ; read next message from rendez-vous
                               (finally (m/? (m/via el)))))
        writef    #(m/sp (try (m/? (write-msg ws %))
                           (finally (m/? (m/via el)))))]
    (run-via el
      ((m/sp
         (try
           (m/? ((p/eval (io/decode (m/? read-task))) ; read and eval photon program sent by client
                 (io/message-writer writef)
                 (io/message-reader read-task)))
           (catch Cancelled _)))
       success failure))))
