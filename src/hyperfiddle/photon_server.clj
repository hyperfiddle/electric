(ns hyperfiddle.photon-server
  "This is a developement server, no SSL support.
  Adapted from `ring.adapter.jetty`."
  (:require [missionary.core :as m]
            [hyperfiddle.logger :as log]
            [cognitect.transit :as t]
            [hyperfiddle.photon :as p])
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
            WebSocketListener WriteCallback SuspendToken]
           [org.eclipse.jetty.websocket.servlet WebSocketCreator WebSocketServlet]
           [clojure.lang IFn]
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.nio ByteBuffer]
           [missionary Cancelled]
           [java.util.concurrent Executors ThreadFactory Executor]))

;;; WEBSOCKETS

(defn nop [])

(defn write-str [^RemoteEndpoint remote ^String message]
  (fn [s f]
    (.sendString remote message
      (reify WriteCallback
        (writeFailed [_ e] (f e))
        (writeSuccess [_] (s nil))))
    nop))

(defn session-suspend! [^Session session]
  (.suspend session))

(defn token-resume! [^SuspendToken token]
  (.resume token))

(def timeouts (atom {}))
(defn set-timeout!
  ([d f] (set-timeout! nil d f))
  ([id d f]
   (let [id (or id (random-uuid))
         clear (m/dfv)
         task (m/sp
                (when (= :proceed (m/? (m/race clear (m/sleep d :proceed))))
                  (f)))]
     (swap! timeouts assoc id clear)
     (future (m/? task) (swap! timeouts dissoc id))
     id)))

(defn clear-timeout! [id] (when-let [clear (get (deref timeouts) id)]
                            (clear :canceled)))

(def sockets (atom {}))

(deftype Ws [client-id
             ^:unsynchronized-mutable cancel
             ^:unsynchronized-mutable remote
             ^:unsynchronized-mutable session
             ^:unsynchronized-mutable msg-str
             ^:unsynchronized-mutable msg-buf
             ^:unsynchronized-mutable close
             ^:unsynchronized-mutable token
             ^:unsynchronized-mutable error]
  IFn
  (invoke [_ _]
    (token-resume! token))
  WebSocketConnectionListener
  (onWebSocketConnect [_ s]
    (if (some? remote)
      (do (log/debug "websocket reconnect")
        (set! session s)
        (reset! remote (.getRemote s)))
      (do (log/debug "websocket connect" {:client-id client-id})
        (set! session s)
        (set! remote (atom (.getRemote s)))
        (set! cancel
          (cancel remote
            (set! msg-str (m/rdv))
            (set! msg-buf (m/rdv))
            (set! close (m/dfv)))))))
  (onWebSocketClose [this s r]
    (log/debug "websocket close" {:client-id client-id :status s, :reason r})
    (set-timeout! client-id
      30000
      (fn []
        (log/debug "websocket did not recover, harvesting session." {:client-id client-id})
        (cancel)
        (close
          (do
            (set! (.close this) nil)
            (set! (.msg-str this) nil)
            (set! (.msg-buf this) nil)
            (merge {:status s :reason r}
              (when-some [e error]
                (set! (.error this) nil)
                {:error e}))))
        (swap! sockets dissoc client-id))))
  (onWebSocketError [_ e]
    (log/error "Websocket error" e)
    (set! error e))
  WebSocketListener
  (onWebSocketText [this msg]
    (if (= "heartbeat" msg)
      (log/trace "heartbeat")
      (do
        (set! token (session-suspend! session))
        ((msg-str msg) this this))))
  (onWebSocketBinary [this payload offset length]
    (log/warn "received binary" {:length length})
    (set! token (session-suspend! session))
    ((msg-buf (ByteBuffer/wrap payload offset length)) this this)))

(defn req->client-id [request] (-> request (.getParameterMap) (get "client-id") first))

(defn- make-reactor! [handler request]
  (let [client-id (req->client-id request)]
    (if-let [socket (get (deref sockets) client-id)]
      (do (clear-timeout! client-id)
        socket)
      (let [socket (->Ws client-id (handler request) nil nil nil nil nil nil nil)]
        (swap! sockets assoc client-id socket)
        socket))))

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

(defn success [client-id exit-value]
  (log/info "websocket handler completed gracefully." {:client-id client-id}))

(defn failure [^Throwable e]
  (.printStackTrace e))

(defn encode "Serialize to transit json" [v]
  (let [out (ByteArrayOutputStream.)]
    (t/write (t/writer out :json) v)
    (.toString out)))
(defn decode "Parse transit json" [^String s] 
  (t/read (t/reader (ByteArrayInputStream. (.getBytes s "UTF-8")) :json)))

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
             (fn [!remote read-str _read-buf _closed]
               ((m/sp (try (loop [] (m/? (write-str (deref !remote) (m/? read-str))) (recur))
                        (catch Cancelled _))) (partial success (req->client-id request)) failure)))
   "/ws" (fn [request]
           (fn [!remote read-str _read-buf _closed]
             (let [el (event-loop (deref !remote))]
               (run-via el
                 ((m/sp
                    (try
                      (let [?read (m/sp
                                    (let [x (try (m/? read-str)
                                              (finally (m/? (m/via el))))]
                                      (try (decode x)
                                        (catch Throwable t
                                          #_(log/error (ex-info "Failed to decode" {:value x} t)) ; don't double log
                                          (throw (ex-info "Failed to decode" {:value x} t))))))
                            write (fn [x]
                                    (m/sp
                                      (try
                                        (m/? (write-str (deref !remote)
                                               (try (encode x)
                                                 (catch Throwable t
                                                   #_(log/error (ex-info "Failed to encode" {:value x} t)) ; don't double log
                                                   (throw (ex-info "Failed to encode" {:value x} t))))))
                                        (finally (m/? (m/via el))))))
                            program (m/? ?read)]
                        (prn :booting-reactor #_program)
                        (m/? ((p/eval program) write ?read)))
                      (catch Cancelled _))) (partial success (req->client-id request)) failure)))))})

#_(defn gzip-handler [& methods]
    (doto (GzipHandler.) (.addIncludedMethods (into-array methods))))

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
                       #_(.setGzipHandler (gzip-handler "GET" "POST"))
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
