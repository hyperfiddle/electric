(ns hyperfiddle.server.websockets
  "@see http://pedestal.io/api/pedestal.jetty/io.pedestal.http.jetty.websockets.html
  @see https://github.com/pedestal/pedestal/blob/09dd88c4ce7f89c7fbb7a398077eb970b3785d2d/samples/jetty-web-sockets/src/jetty_web_sockets/service.clj"
  (:require ;; [hypercrud.transit :as hc-t] ;; TODO restore
            ;; [hyperfiddle.service.auth :as auth] ;; TODO restore
            [io.pedestal.http.jetty.websockets :as ws]
            [clojure.core.async :as a]
            ;; [promesa.core :as p] ;; TODO drop
            [clojure.edn :as edn]
            [hfdl.lang :as hfdl]
            [hyperfiddle.server.entrypoint :as entrypoint]
            [missionary.core :as m]
            [taoensso.timbre :as log])
  (:import javax.servlet.Servlet
           [org.eclipse.jetty.servlet ServletContextHandler ServletHolder]
           [org.eclipse.jetty.websocket.api RemoteEndpoint Session WebSocketConnectionListener WebSocketListener]))

(defn- send! [context data & [close?]]
  (let [chan (get-in context [:ws :chan])]
    (a/go
      (a/>! chan (pr-str data) #_(hc-t/encode data))
      (when close?
        (a/<! (a/timeout 1000))
        (a/close! chan)))))

(defn- build-context [context session chan]
  (update context :ws assoc :session session, :chan chan))

(defn- handle! [{:keys [!route process] :as context} {:keys [id type data]}]
  (try
    (case type
      :ping       (send! context {:id id, :type :pong})         ; heartbeat
      :goodbye    (send! context {:id id, :type :goodbye} true) ; graceful shutdown
      :set-route! (do (reset! !route data)
                      (send! context {:id   id
                                      :type :result
                                      :data (hfdl/result @process)})))
    (catch Throwable t
      (log/error t)
      (send! context {:id id, :type :error, :message (ex-message t)}))))

(defn- on-text [context message]
  (try
    (handle! context (edn/read-string message) #_(hc-t/decode message)) ;; TODO use edamame
    (catch Throwable t
      (log/error t)
      (send! context {:type :error, :message (ex-message t)}))))

(defn- on-binary [_context payload _offset _length]
  (log/info :msg "Binary Message!" :bytes payload))

(defn- on-error [_context t]
  (log/error :msg "WS Error happened" :exception t))

(defn on-close [_context _num-code reason-text]
  (log/info :msg "WS Closed:" :reason reason-text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn start-ws-connection
  "Given a function of two arguments
  (the Jetty WebSocket Session and its paired core.async 'send' channel),
  and optionall a buffer-or-n for the 'send' channel,
  return a function that can be used as an OnConnect handler.

  Notes:
   - You can control the entire WebSocket Session per client with the
  session object.
   - If you close the `send` channel, Pedestal will close the WS connection."
  ([on-connect-fn]
   (start-ws-connection on-connect-fn 10))
  ([on-connect-fn send-buffer-or-n]
   (fn start-ws-connection [context ^Session ws-session]
     (let [send-ch (a/chan send-buffer-or-n)
           remote  ^RemoteEndpoint (.getRemote ws-session)]
       ;; Let's process sends...
       (a/go-loop []
         (if-let [out-msg (and (.isOpen ws-session)
                               (a/<! send-ch))]
           (do (ws/ws-send out-msg remote)
               (recur))
           (.close ws-session)))
       (on-connect-fn context ws-session send-ch)))))

(defn build-ws-context [{:keys [config request]}]
  (let [request (bean request)
        !route  (atom nil)]
    (try
      {:config  config
       :request request
       :!route  !route
       :process (entrypoint/eval-fiddle! (m/watch !route))
       ;; :auth    (auth/build-auth-context config request) ;; TODO restore
       :ws      {:session nil
                 :chan    nil}}
      (catch Throwable t
        (log/error (ex-info "Failed to initialize socket context" {} t))
        (throw t)))))

(defn make-ws-listener
  "Given a map representing WebSocket actions
  (:on-connect, :on-close, :on-error, :on-text, :on-binary),
  return a WebSocketConnectionListener.
  Values for the map are functions with the same arity as the interface."
  [config req _res ws-map]
  (let [context (build-ws-context {:config  config, :request req})]
    ;; TODO re-enable auth
    (if false #_(and (auth/configured? context)
             (not (auth/authenticated? context)))
      nil ;; Kick the user out ;; TODO find a way to send back a proper 401
      (let [context   (volatile! context)
            pristine? (volatile! true)]
        (reify
          WebSocketConnectionListener
          (onWebSocketConnect [this ws-session]
            (assert @pristine?) ;; No reconnect allowed.
            (when-let [f (:on-connect ws-map)]
              (vreset! pristine? false)
              (vreset! context (f @context ws-session))))
          (onWebSocketClose [this status-code reason]
            (when-let [f (:on-close ws-map)]
              (f @context status-code reason)))
          (onWebSocketError [this cause]
            (when-let [f (:on-error ws-map)]
              (f @context cause)))

          WebSocketListener
          (onWebSocketText [this msg]
            (when-let [f (:on-text ws-map)]
              (f @context msg)))
          (onWebSocketBinary [this payload offset length]
            (when-let [f (:on-binary ws-map)]
              (f @context payload offset length))))))))

(defn add-ws-endpoints
  "Given a ServletContextHandler and a map of WebSocket (String) paths to action maps,
  produce corresponding Servlets per path and add them to the context.
  Return the context when complete.

  You may optionally also pass in a map of options.
  Currently supported options:
   :listener-fn - A function of 3 args,
                  the ServletUpgradeRequest, ServletUpgradeResponse, and the WS-Map
                  that returns a WebSocketListener."
  ([config ^ServletContextHandler ctx ws-paths]
   (doseq [[path ws-map] ws-paths]
     (let [servlet (ws/ws-servlet (fn [req response]
                                    (make-ws-listener config req response ws-map)))]
       (.addServlet ctx (ServletHolder. ^Servlet servlet) path)))
   ctx))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def paths
  {"/ws" {:on-connect (start-ws-connection #'build-context)
          :on-text    #'on-text
          :on-binary  #'on-binary
          :on-error   #'on-error
          :on-close   #'on-close}})
