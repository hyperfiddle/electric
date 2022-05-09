(ns hyperfiddle.server.websockets
  (:require
    [missionary.core :as m]
    [hyperfiddle.dev.logger :as log])
  (:import (org.eclipse.jetty.servlet ServletContextHandler ServletHolder)
           (org.eclipse.jetty.websocket.api RemoteEndpoint Session WebSocketConnectionListener
                                            WebSocketListener WriteCallback SuspendToken)
           (org.eclipse.jetty.websocket.servlet WebSocketCreator WebSocketServlet)
           (clojure.lang IFn)
           (java.nio ByteBuffer)))

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
   (let [id    (or id (random-uuid))
         clear (m/dfv)
         task  (m/sp
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
      (do (log/debug "websocket connect")
          (set! session s)
          (set! remote (atom (.getRemote s)))
          (set! cancel
                (cancel remote
                        (set! msg-str (m/rdv))
                        (set! msg-buf (m/rdv))
                        (set! close (m/dfv)))))))
  (onWebSocketClose [this s r]
    (log/debug "websocket close" {:status s, :reason r})
    (set-timeout! client-id
                  30000
                  (fn []
                    (log/debug "websocket did not recover, canceling reactor and cleaning up." {:client-id client-id})
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

(defn- make-reactor! [handler request]
  (let [client-id (-> request (.getParameterMap) (get "client-id") first)]
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
                    (createWebSocket [_ request response]
                      ;; TODO re-enable auth
                      #_(auth/build-auth-context config request)
                      (when (and
                              #_(auth/configured? context)
                              #_(auth/authenticated? context))
                        (make-reactor! handler request))))))))
          ^String path)))))
