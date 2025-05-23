(ns hyperfiddle.electric-jetty9-ring-adapter3
  "An electric jetty9 adapter suitable for use with the official ring adapter"
  (:require [clojure.tools.logging :as log]
            [missionary.core :as m]
            [hyperfiddle.electric.impl.sunng87-ring-jetty9-ws-adapter :as ws-adapter]
            [hyperfiddle.electric.impl.runtime3 :as r])
  (:import [missionary Cancelled]
           [java.nio ByteBuffer]
           [java.util.concurrent.atomic AtomicInteger]
           [org.eclipse.jetty.server Handler]
           [org.eclipse.jetty.server.handler ContextHandler HandlerList]
           [org.eclipse.jetty.websocket.api Session WebSocketAdapter]))

(def proxy-ws-handler ws-adapter/proxy-ws-handler)

(def send! ws-adapter/send!)
(def req-of ws-adapter/req-of)

(defn failure [^WebSocketAdapter ws ^Throwable e]
  (if (instance? Cancelled e)
    (log/debug "Websocket handler completed gracefully.")
    ;; jetty/close! is missing arity 3 for jetty 9. Call close directly to get arity 3.
    (when-some [s (.getSession ws)]
      (let [{::keys [type time-seconds] :as data} (ex-data e)]
        (case type
          ::timeout (do (log/info (format "Connection to client lost after %ss. Closing socket." time-seconds))
                        (.close s 1013 "Try again later"))
          (do (log/error e "Websocket handler failure." data)
              (.close s 1011 "Server process crash")))))))

(defn send-async! [this msg ack crash] (ws-adapter/send! this msg {:write-success ack, :write-failed crash}))

(defn write-msgs
  "Returns a task writing all messages emitted by flow on websocket."
  [socket msgs]
  (fn [s f]
    (let [slot-ps 0
          slot-done 1
          slot-error 2
          slots (object-array 3)
          state (AtomicInteger.)]
      (letfn [(ready []
                (if (aget slots slot-done)
                  (if-some [e (aget slots slot-error)] (f e) (s nil))
                  (if (nil? (aget slots slot-error))
                    (try (send-async! socket @(aget slots slot-ps) ack crash)
                         (catch Throwable e (crash e)))
                    (do (try @(aget slots slot-ps) (catch Throwable _))
                        (ack)))))
              (ack [] (when (zero? (.decrementAndGet state)) (ready)))
              (crash [e]
                (aset slots slot-error e)
                (cancel) (ack))
              (cancel [] ((aget slots slot-ps)))]
        (aset slots slot-done false)
        (aset slots slot-ps
          (msgs #(when (zero? (.incrementAndGet state)) (ready))
            #(do (aset slots slot-done true)
                 (when (zero? (.incrementAndGet state)) (ready)))))
        (ack) cancel))))

(defn timeout
  "Throw if `mailbox` haven't got any message after given `time` ms"
  [mailbox time]
  (m/sp
    (loop []
      (when (= :timeout (m/? (m/timeout mailbox time :timeout)))
        (throw (ex-info "No message received after specified time" {::type ::timeout, ::time-seconds (int (/ time 1000))})))
      (recur))))

(defn send-hf-heartbeat [delay send!]
  (m/sp (loop [] (m/? (m/sleep delay)) (send! "HEARTBEAT") (recur))))

(def ELECTRIC-CONNECTION-TIMEOUT 59000) ; https://www.notion.so/hyperfiddle/electric-server-heartbeat-issues-4243f981954c419f8eb0785e8e789fb7?pvs=4
(def ELECTRIC-HEARTBEAT-INTERVAL 45000)

(defn electric-jetty9-ws-adapter
  "Start and manage an Electric server process hooked onto a websocket."
  ([boot-fn] (electric-jetty9-ws-adapter boot-fn nil))
  ([boot-fn ring-req] ; optional ring-req is for debugging
   (let [state             (object-array 2)
         on-message-slot   (int 0)
         on-close-slot     (int 1)
         keepalive-mailbox (m/mbx)]
     {:on-connect (fn on-connect [^WebSocketAdapter ws]
                    (log/debug "WS connect" ring-req)
                    (.setMaxTextMessageSize (.getPolicy (.getSession ws)) (* 100 1024 1024))  ; Allow large value payloads, temporary.
                    (aset state on-close-slot
                      ((m/join (fn [& _])
                         (timeout keepalive-mailbox ELECTRIC-CONNECTION-TIMEOUT)
                         (write-msgs ws ((boot-fn) (r/subject-at state on-message-slot)))
                         (send-hf-heartbeat ELECTRIC-HEARTBEAT-INTERVAL #(send! ws %)))
                       {} (partial failure ws)))) ; Start Electric process
      :on-close   (fn on-close [ws status-code reason]
                    (let [status {:status status-code, :reason reason}]
                      (case (long status-code) ; https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4.1
                        1000 (log/debug "Client disconnected gracefully" status)
                        1001 (log/debug "Client navigated away" status)
                        ;; 1005 is the default close code set by Chrome and FF unless specified.
                        1005 (log/debug "Client disconnected for an unknown reason (browser default close code)" status)
                        (log/debug "Client disconnected for an unexpected reason." status))
                      ((aget state on-close-slot))))
      :on-error   (fn on-error [ws err]
                    (log/error err "Websocket error"))
      :on-ping    (fn on-ping [^WebSocketAdapter ws bytebuffer] ; Send pong and keep connection alive.
                    (.sendPong (.getRemote (.getSession ws)) bytebuffer)
                    (keepalive-mailbox nil))
      :on-pong    (fn on-pong [ws bytebuffer] ; ignore pong, no use case
                    (log/trace "pong"))
      :on-text    (fn on-text [^WebSocketAdapter ws text]
                    (keepalive-mailbox nil)
                    (log/trace "text received" text)
                    (when-not (= "HEARTBEAT" text)
                      ((aget state on-message-slot) text)))
      :on-bytes   (fn [^WebSocketAdapter ws ^bytes bytes offset length]
                    (keepalive-mailbox nil)
                    (log/trace "bytes received" {:length length})
                    ((aget state on-message-slot) (ByteBuffer/wrap bytes offset length)))})))

(defn reject-websocket-handler
  "Will accept socket connection upgrade and immediately close the socket on
  connection, with given `code` and `reason`. Use this to cleanly reject a
  websocket connection."
  ;; Rejecting the HTTP 101 Upgrade request would also prevent the socket to
  ;; open, but for security reasons, the client is never informed of the HTTP
  ;; 101 failure cause.
  [code reason]
  {:on-connect (fn [^WebSocketAdapter ws] (.. ws getSession (close code reason)))})

#_(def VERSION (not-empty (System/getProperty "ELECTRIC_USER_VERSION"))) ; see Dockerfile
#_(defn wrap-reject-stale-client
    "Intercept websocket UPGRADE request and check if client and server versions matches.
    An electric client is allowed to connect if its version matches the server's version, or if the server doesn't have a version set (dev mode).
    Otherwise, the client connection is rejected gracefully."
    [next-handler]
    (fn [ring-req]
      (let [client-version (get-in ring-req [:query-params "ELECTRIC_USER_VERSION"])]
        (cond
          (nil? VERSION)             (next-handler ring-req)
          (= client-version VERSION) (next-handler ring-req)
          :else (adapter/reject-websocket-handler 1008 "stale client") ; https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1
          ))))

(defn electric-jetty9-ws-install "Use under `:configurator` key of `ring.adapter.jetty/run-jetty` to install an electric websocket handler.

  With no middleware

  ```clj
  (electric-jetty9-ws-install jetty-server \"/\" entrypoint)
  ```

  where `entrypoint` is e.g. `(fn [ring-req] (e/boot-server {} electric-starter-app.main/Main (e/server ring-req)))`.

  Pass `middleware` to enrich the handler with custom middleware.

  Passes optional `config` forward to `proxy-ws-handler`.
"
  ([jetty-server path entrypoint] (electric-jetty9-ws-install jetty-server path entrypoint identity))
  ([jetty-server path entrypoint middleware] (electric-jetty9-ws-install jetty-server path entrypoint middleware {}))
  ([jetty-server path entrypoint middleware
    {:as config :keys [ws-max-idle-time ws-max-text-message-size] :or {ws-max-idle-time 500000 ws-max-text-message-size 65536}}] ; copied from proxy-ws-handler for documentation
   (letfn [(create-websocket-handler [context-path handler]
             (doto (ContextHandler.)
               (.setContextPath context-path) ; matches e.g. "/"
               (.setAllowNullPathInfo false) ; FIXME can we remove this? not sure what it does for just "/". It's really up to the user to canonicalize urls. https://javadoc.jetty.org/jetty-9/org/eclipse/jetty/server/handler/ContextHandler.html#setAllowNullPathInfo(boolean)
               (.setHandler (proxy-ws-handler handler config))))
           (add-websocket-handler [server path handler]
             (let [handlers [(create-websocket-handler path handler) (.getHandler server)]]
               (.setHandler server (doto (HandlerList.) (.setHandlers (into-array Handler handlers))))))]
     (doto jetty-server
       (add-websocket-handler path (middleware (fn [ring-req] (electric-jetty9-ws-adapter (partial entrypoint ring-req) ring-req))))))))
