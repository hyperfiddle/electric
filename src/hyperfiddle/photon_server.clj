(ns hyperfiddle.photon-server
  "This is a developement server, no SSL support.
  Adapted from `ring.adapter.jetty`."
  (:require [missionary.core :as m]
            [hyperfiddle.logger :as log]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-impl.io :as io])
  (:import [org.eclipse.jetty.server
            Request
            Server
            ServerConnector
            ConnectionFactory
            HttpConfiguration
            HttpConnectionFactory]
           [org.eclipse.jetty.server.handler AbstractHandler]
           [org.eclipse.jetty.util BlockingArrayQueue]
           [org.eclipse.jetty.util.thread ThreadPool QueuedThreadPool]
           [javax.servlet DispatcherType]
           [javax.servlet.http HttpServletRequest]
           [org.eclipse.jetty.servlet ServletContextHandler ServletHolder]
           [org.eclipse.jetty.websocket.api RemoteEndpoint Session WebSocketConnectionListener
            WebSocketPingPongListener WebSocketListener WriteCallback SuspendToken]
           [org.eclipse.jetty.websocket.servlet WebSocketCreator WebSocketServlet]
           [clojure.lang IFn]
           [java.nio ByteBuffer]
           [missionary Cancelled]
           [java.util.concurrent Executors ThreadFactory Executor]))

;;; WEBSOCKETS

(defn nop [])

(defn write-msg [^RemoteEndpoint remote message]
  (fn [s f]
    (let [cb (reify WriteCallback
               (writeFailed [_ e] (f e))
               (writeSuccess [_] (s nil)))]
      (if (string? message)
        (.sendString remote ^String message cb)
        (.sendBytes remote ^ByteBuffer message cb)))
    nop))

(defn session-suspend! [^Session session]
  (.suspend session))

(defn token-resume! [^SuspendToken token]
  (.resume token))

(defn make-heartbeat [^Session s pong-mailbox]
  ;; Ping wisely https://developer.android.com/training/connectivity/network-access-optimization#RadioStateMachine
  (m/sp (loop []
          (m/? (m/sleep 15000))
          (.sendPing (.getRemote s) (ByteBuffer/allocate 0))
          (let [pong (m/? (m/timeout pong-mailbox 20000 :timeout))]
            (if (= :timeout pong)
              (.disconnect s)
              (recur))))))

(deftype Ws [^:unsynchronized-mutable cancel
             ^:unsynchronized-mutable remote
             ^:unsynchronized-mutable session
             ^:unsynchronized-mutable messages
             ^:unsynchronized-mutable close
             ^:unsynchronized-mutable token
             ^:unsynchronized-mutable error
             ^:unsynchronized-mutable heartbeat
             ^:unsynchronized-mutable pong-mailbox]
  IFn
  (invoke [_ _]
    (token-resume! token))
  WebSocketConnectionListener
  (onWebSocketConnect [this s]
    (do (log/debug "websocket connect")
      (set! session s)
      (set! remote (.getRemote s))
      (set! cancel
        (cancel remote
          (set! messages (m/rdv))
          (set! close (m/dfv))))
      (set! pong-mailbox (m/mbx))
      (set! heartbeat ((make-heartbeat s pong-mailbox) (fn [_]) (fn [_])))))
  (onWebSocketClose [this status reason]
    (letfn [(close! []
              (heartbeat) ; cancel heartbeat
              (cancel) ; FIXME success or failure callback not called
              (close (do (set! (.close this) nil)
                         (set! (.-messages this) nil)
                       (merge {:status status, :reason reason}
                         (when-some [e (.error this)]
                           (set! (.error this) nil)
                           {:error e})))))]
      (case status ; https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent/code
        (1000 1001) (do (log/info "Client disconnected")
                      (close!))
        (do (log/error "Client socket disconnected for an unexpected reason." {:status status, :reason reason})
          (close!)))))
  (onWebSocketError [_ e]
    (log/error "Websocket error" e)
    (set! error e))
  WebSocketPingPongListener
  (onWebSocketPing ^void [this data]
    (.sendPong remote data))
  (onWebSocketPong [this data]
    (pong-mailbox data)) ; Attack surface, mailbox could overflow if server is flooded with pong frames.
  WebSocketListener
  (onWebSocketText [this msg]
    (set! token (session-suspend! session))
    ((messages msg) this this))
  (onWebSocketBinary [this payload offset length]
    (set! token (session-suspend! session))
    ((messages (ByteBuffer/wrap payload offset length)) this this)))

(defn- make-reactor! [handler request]
  (->Ws (handler request) nil nil nil nil nil nil nil nil))

(def add-ws-endpoints
  (partial reduce-kv
    (fn [^ServletContextHandler ctx ^String path handler]
      (doto ctx
        (.addServlet
          (ServletHolder.
            (proxy [WebSocketServlet] []
              (configure [factory]
                (.setCreator factory
                  (reify WebSocketCreator
                    (createWebSocket [_ request _response]
                      ;; TODO re-enable auth
                      #_(auth/build-auth-context config request)
                      (when (and
                              #_(auth/configured? context)
                              #_(auth/authenticated? context))
                        (make-reactor! handler request))))))))
          ^String path)))))

(def event-loop
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

(defn run-via [^Executor e f]
  (fn [& args]
    (.execute e
      (reify Runnable
        (run [_]
          (apply f args))))))

(defn success [exit-value]
  (log/info "websocket handler completed gracefully."))

(defn failure [^Throwable e]
  (log/error e) #_(.printStackTrace e))

;; to boot a distributed photon app
;; p/main is expanded on cljs side, returns a pair
;; first element is the compiled client version
;; second element is the server version as data.
;; When client starts, it initializes the ws connection, then sends the server program as first message, the boots its
;; local reactor.
;; The server listens to incoming ws connections, waits for the program as first message, the evaluates the program and
;; boots its local reactor.

(def ws-paths
  {"/echo" (fn [request]
             (fn [remote read-msg _closed]
               ((m/sp (try (loop [] (m/? (write-msg remote (m/? read-msg))) (recur))
                           (catch Cancelled _))) success failure)))
   "/ws"   (fn [request]
             (fn [remote read-msg _closed]
               (let [el (event-loop remote)
                     r (m/sp (try (m/? read-msg)
                                  (finally (m/? (m/via el)))))
                     w #(m/sp (try (m/? (write-msg remote %))
                                   (finally (m/? (m/via el)))))]
                 (run-via el
                   ((m/sp
                      (try
                        (m/? ((p/eval (io/decode (m/? r)))
                              (io/message-writer w)
                              (io/message-reader r)))
                        (catch Cancelled _)))
                    success failure)))))})

;;; HTTP Server

(defn- proxy-handler ^AbstractHandler [handler]
  (proxy [AbstractHandler] []
    (handle [_ ^Request base-request ^HttpServletRequest request response]
      (when-not (= (.getDispatcherType request) DispatcherType/ERROR)
        (handler base-request) ;; response available with getResponse
        (.setHandled base-request true)))))

(defn- server-connector ^ServerConnector [^Server server & factories]
  (ServerConnector. server #^"[Lorg.eclipse.jetty.server.ConnectionFactory;" (into-array ConnectionFactory factories)))

(defn- http-config ^HttpConfiguration [options]
  (doto (HttpConfiguration.)
    (.setSendDateHeader (:send-date-header? options true))
    (.setOutputBufferSize (:output-buffer-size options 32768))
    (.setRequestHeaderSize (:request-header-size options 8192))
    (.setResponseHeaderSize (:response-header-size options 8192))
    (.setSendServerVersion (:send-server-version? options true))))

(defn- http-connector ^ServerConnector [server options]
  (let [http-factory (HttpConnectionFactory. (http-config options))]
    (doto (server-connector server http-factory)
      (.setPort (options :port 80))
      (.setHost (options :host))
      (.setIdleTimeout (options :max-idle-time 200000)))))

(defn- create-threadpool ^ThreadPool [options]
  (let [min-threads (options :min-threads 8)
        max-threads (options :max-threads 50)
        queue-max-capacity (-> (options :max-queued-requests Integer/MAX_VALUE) (max 8))
        queue-capacity (-> min-threads (max 8) (min queue-max-capacity))
        blocking-queue (BlockingArrayQueue. queue-capacity
                         queue-capacity
                         queue-max-capacity)
        thread-idle-timeout (options :thread-idle-timeout 60000)
        pool (QueuedThreadPool. max-threads
               min-threads
               thread-idle-timeout
               blocking-queue)]
    (when (:daemon? options false)
      (.setDaemon pool true))
    pool))

(defn- create-server ^Server [options]
  (let [pool (or (:thread-pool options) (create-threadpool options))
        server (Server. pool)]
    (when (:http? options true)
      (.addConnector server (http-connector server options)))
    server))

(defn run-jetty
  ^Server [handler options]
  (let [server (create-server (dissoc options :configurator))]
    (.setHandler server (proxy-handler handler))
    (when-let [configurator (:configurator options)]
      (configurator server))
    (try
      (.start server)
      (when (:join? options true)
        (.join server))
      server
      (catch Exception ex
        (.stop server)
        (throw ex)))))

;; assets served on 8080 by shadow
;; websocket served on 8081 by Jetty
(def default-config
  {:host "localhost"
   :port 8082
   :join? false
   :configurator (fn [^Server server]
                   (let [context (new ServletContextHandler)]
                     (doto context
                       (add-ws-endpoints ws-paths))
                     (.setHandler server context)))})

(defn start! ^Server 
  ([] (start! nil))
  ([config]
   (run-jetty (constantly nil)
     (merge default-config config))))

(defn stop! [^Server server]
  (.stop server)
  server)
