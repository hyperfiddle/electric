(ns hyperfiddle.electric.impl.sunng87-ring-jetty9-ws-adapter "
Copyright © 2013-2024 Sun Ning; EPL v1.
inlined websocket subset only with light modifications from
https://github.com/sunng87/ring-jetty9-adapter/"
  (:import [org.eclipse.jetty.server Request Response]
           [org.eclipse.jetty.server.handler AbstractHandler]
           [org.eclipse.jetty.websocket.api
            WebSocketAdapter Session
            UpgradeRequest RemoteEndpoint WriteCallback
            WebSocketPingPongListener]
           [org.eclipse.jetty.websocket.api.extensions ExtensionConfig]
           [org.eclipse.jetty.websocket.server WebSocketHandler]
           [org.eclipse.jetty.websocket.servlet
            WebSocketServletFactory WebSocketCreator
            ServletUpgradeRequest ServletUpgradeResponse]
           [clojure.lang IFn]
           [javax.servlet.http HttpServletRequest HttpServletResponse]
           [java.nio ByteBuffer]
           [java.util Locale])
  (:require [clojure.string :as string]
            [ring.util.servlet :as servlet]))

(defprotocol RequestMapDecoder
  (build-request-map [r]))

(defn set-headers
  "Update a HttpServletResponse with a map of headers."
  [^HttpServletResponse response, headers]
  (doseq [[key val-or-vals] headers]
    (if (string? val-or-vals)
      (.setHeader response key val-or-vals)
      (doseq [val val-or-vals]
        (.addHeader response key val))))
  ; Some headers must be set through specific methods
  (when-let [content-type (get headers "Content-Type")]
    (.setContentType response content-type)))

(defn get-headers
  "Creates a name/value map of all the request headers."
  [^HttpServletRequest request]
  (reduce
    (fn [headers, ^String name]
      (assoc headers
        (.toLowerCase name Locale/ENGLISH)
        (->> (.getHeaders request name)
             (enumeration-seq)
             (string/join ","))))
    {}
    (enumeration-seq (.getHeaderNames request))))

(defprotocol WebSocketProtocol
  (send! [this msg] [this msg callback])
  (ping! [this] [this msg])
  (close! [this])
  (remote-addr [this])
  (idle-timeout! [this ms])
  (connected? [this])
  (req-of [this]))

(defprotocol WebSocketSend
  (-send! [x ws] [x ws callback] "How to encode content sent to the WebSocket clients"))

(defprotocol WebSocketPing
  (-ping! [x ws] "How to encode bytes sent with a ping"))

(def ^:private no-op (constantly nil))

(defn- write-callback
  [{:keys [write-failed write-success]
    :or {write-failed  no-op
         write-success no-op}}]
  (reify WriteCallback
    (writeFailed [_ throwable]
      (write-failed throwable))
    (writeSuccess [_]
      (write-success))))

(extend-protocol WebSocketSend
  (Class/forName "[B")
  (-send!
    ([ba ws]
     (-send! (ByteBuffer/wrap ba) ws))
    ([ba ws callback]
     (-send! (ByteBuffer/wrap ba) ws callback)))

  ByteBuffer
  (-send!
    ([bb ws]
     (-> ^WebSocketAdapter ws .getRemote (.sendBytes ^ByteBuffer bb)))
    ([bb ws callback]
     (-> ^WebSocketAdapter ws .getRemote (.sendBytes ^ByteBuffer bb ^WriteCallback (write-callback callback)))))

  String
  (-send!
    ([s ws]
     (-> ^WebSocketAdapter ws .getRemote (.sendString ^String s)))
    ([s ws callback]
     (-> ^WebSocketAdapter ws .getRemote (.sendString ^String s ^WriteCallback (write-callback callback)))))

  IFn
  (-send! [f ws]
    (-> ^WebSocketAdapter ws .getRemote f))

  Object
  (send!
    ([this ws]
     (-> ^WebSocketAdapter ws .getRemote
         (.sendString ^RemoteEndpoint (str this))))
    ([this ws callback]
     (-> ^WebSocketAdapter ws .getRemote
         (.sendString ^RemoteEndpoint (str this) ^WriteCallback (write-callback callback))))))

(extend-protocol WebSocketPing
  (Class/forName "[B")
  (-ping! [ba ws] (-ping! (ByteBuffer/wrap ba) ws))

  ByteBuffer
  (-ping! [bb ws] (-> ^WebSocketAdapter ws .getRemote (.sendPing ^ByteBuffer bb)))

  String
  (-ping! [s ws] (-ping! (.getBytes ^String s) ws))

  Object
  (-ping! [o ws] (-ping! (str o) ws)))

(extend-protocol RequestMapDecoder
  ServletUpgradeRequest
  (build-request-map [request]
    (let [servlet-request (.getHttpServletRequest request)
          base-request-map {:server-port (.getServerPort servlet-request)
                            :server-name (.getServerName servlet-request)
                            :remote-addr (.getRemoteAddr servlet-request)
                            :uri (.getRequestURI servlet-request)
                            :query-string (.getQueryString servlet-request)
                            :scheme (keyword (.getScheme servlet-request))
                            :request-method (keyword (.toLowerCase (.getMethod servlet-request) Locale/ENGLISH))
                            :protocol (.getProtocol servlet-request)
                            :headers (get-headers servlet-request)
                            :ssl-client-cert (first (.getAttribute servlet-request
                                                                   "javax.servlet.request.X509Certificate"))}]
      (assoc base-request-map
             :websocket-subprotocols (into [] (.getSubProtocols request))
             :websocket-extensions (into [] (.getExtensions request))))))

(extend-protocol WebSocketProtocol
  WebSocketAdapter
  (send!
    ([this msg]
     (-send! msg this))
    ([this msg callback]
     (-send! msg this callback)))
  (ping!
    ([this]
     (-ping! (ByteBuffer/allocate 0) this))
    ([this msg]
     (-ping! msg this)))
  (close! [this]
    (.. this (getSession) (close)))
  (remote-addr [this]
    (.. this (getSession) (getRemoteAddress)))
  (idle-timeout! [this ms]
    (.. this (getSession) (setIdleTimeout ^long ms)))
  (connected? [this]
    (. this (isConnected)))
  (req-of [this]
    (build-request-map (.. this (getSession) (getUpgradeRequest)))))

(defn- proxy-ws-adapter
  [{:as ws-fns
    :keys [on-connect on-error on-text on-close on-bytes on-ping on-pong]
    :or {on-connect no-op
         on-error no-op
         on-text no-op
         on-close no-op
         on-bytes no-op
         on-ping no-op
         on-pong no-op}}]
  (proxy [WebSocketAdapter WebSocketPingPongListener] []
    (onWebSocketConnect [^Session session]
      (let [^WebSocketAdapter this this]
        (proxy-super onWebSocketConnect session))
      (on-connect this))
    (onWebSocketError [^Throwable e]
      (on-error this e))
    (onWebSocketText [^String message]
      (on-text this message))
    (onWebSocketClose [statusCode ^String reason]
      (let [^WebSocketAdapter this this]
        (proxy-super onWebSocketClose statusCode reason))
      (on-close this statusCode reason))
    (onWebSocketBinary [^bytes payload offset len]
      (on-bytes this payload offset len))
    (onWebSocketPing [^ByteBuffer bytebuffer]
      (on-ping this bytebuffer))
    (onWebSocketPong [^ByteBuffer bytebuffer]
      (on-pong this bytebuffer))))

(defn- reify-default-ws-creator
  [ws-fns]
  (reify WebSocketCreator
    (createWebSocket [this _ _]
      (proxy-ws-adapter ws-fns))))

(defn- reify-custom-ws-creator
  [ws-creator-fn]
  (reify WebSocketCreator
    (createWebSocket [this req resp]
      (let [req-map (build-request-map req)
            ws-results (ws-creator-fn req-map)]
        (if-let [{:keys [code message headers]} (:error ws-results)]
          (do (set-headers resp headers)
              (.sendError resp code message))
          (do
            (when-let [sp (:subprotocol ws-results)]
              (.setAcceptedSubProtocol resp sp))
            (when-let [exts (not-empty (:extensions ws-results))]
              (.setExtensions resp (mapv #(ExtensionConfig. ^String %) exts)))
            (proxy-ws-adapter ws-results)))))))

(defn ^:internal proxy-ws-handler
  "Returns a Jetty websocket handler"
  [ws {:as options
       :keys [ws-max-idle-time
              ws-max-text-message-size]
       :or {ws-max-idle-time 500000
            ws-max-text-message-size 65536}}]
  (proxy [WebSocketHandler] []
    (configure [^WebSocketServletFactory factory]
      (doto (.getPolicy factory)
        (.setIdleTimeout ws-max-idle-time)
        (.setMaxTextMessageSize ws-max-text-message-size))
      (.setCreator factory
                   (if (map? ws)
                     (reify-default-ws-creator ws)
                     (reify-custom-ws-creator ws))))
    (handle [^String target, ^Request request req ^Response res]
      (let [^WebSocketHandler this this
            ^WebSocketServletFactory wsf (proxy-super getWebSocketFactory)]
        (if (.isUpgradeRequest wsf req res)
          (if (.acceptWebSocket wsf req res)
            (.setHandled request true)
            (when (.isCommitted res)
              (.setHandled request true)))
          (proxy-super handle target request req res))))))
