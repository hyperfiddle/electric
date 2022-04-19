(ns hyperfiddle.server.websockets
  (:require
    ;; [hypercrud.transit :as hc-t] ;; TODO restore
    ;; [hyperfiddle.service.auth :as auth] ;; TODO restore
    [missionary.core :as m]
    [hyperfiddle.dev.logger :as log])
  (:import (org.eclipse.jetty.servlet ServletContextHandler ServletHolder)
           (org.eclipse.jetty.websocket.api RemoteEndpoint Session WebSocketConnectionListener
                                            WebSocketListener WriteCallback SuspendToken)
           (org.eclipse.jetty.websocket.servlet WebSocketCreator WebSocketServlet)
           (clojure.lang IFn)
           (java.nio ByteBuffer)))

(defn close! [^Session session]
  (.close session))

(defn open? [^Session session]
  (.isOpen session))

(defn nop [])

(defn write-str [^RemoteEndpoint remote ^String message]
  (fn [s f]
    (.sendString remote message
      (reify WriteCallback
        (writeFailed [_ e] (f e))
        (writeSuccess [_] (s nil))))
    nop))

(defn write-buf [^RemoteEndpoint remote ^ByteBuffer message]
  (fn [s f]
    (.sendBytes remote message
      (reify WriteCallback
        (writeFailed [_ e] (f e))
        (writeSuccess [_] (s nil))))
    nop))

(defn session-suspend! [^Session session]
  (.suspend session))

(defn token-resume! [^SuspendToken token]
  (.resume token))

(deftype Ws [^:unsynchronized-mutable cancel
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
    (log/debug "websocket connect")
    (set! session s)
    (set! cancel
      (cancel (.getRemote s)
        (set! msg-str (m/rdv))
        (set! msg-buf (m/rdv))
        (set! close (m/dfv)))))
  (onWebSocketClose [_ s r]
    (log/debug "websocket close")
    (cancel)
    (close
      (do
        (set! close nil)
        (set! msg-str nil)
        (set! msg-buf nil)
        (merge {:status s :reason r}
          (when-some [e error]
            (set! error nil)
            {:error e})))))
  (onWebSocketError [_ e]
    (log/error e)
    (set! error e))
  WebSocketListener
  (onWebSocketText [this msg]
    (log/trace "receive text" msg)
    (if (= "heartbeat" msg)
      (log/trace "heartbeat")
      (do
        (set! token (session-suspend! session))
        ((msg-str msg) this this))))
  (onWebSocketBinary [this payload offset length]
    (log/warn "received binary" {:length length})
    (set! token (session-suspend! session))
    ((msg-buf (ByteBuffer/wrap payload offset length)) this this)))

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
                              #_(auth/authenticated? context)
                              )
                        (->Ws (handler request) nil nil nil nil nil nil))))))))
          ^String path)))))
