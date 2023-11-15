(ns hyperfiddle.electric-httpkit-adapter
  (:require
   [clojure.tools.logging :as log]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric.impl.io :as io]
   [hyperfiddle.electric.impl.runtime :as r]
   [missionary.core :as m]
   [org.httpkit.server :as http-kit])
  (:import
   (java.nio ByteBuffer)
   (missionary Cancelled)
   (org.httpkit.server AsyncChannel)))

(defn failure [^AsyncChannel ch ^Throwable e]
  (if (instance? Cancelled e)
    (prn "Websocket handler completed gracefully.")
    ;; http-kit doesn’t support reason messages, only status code.
    (let [{::keys [type time-seconds] :as data} (ex-data e)]
      (case type
        ::timeout (do (log/info (format "Connection to client lost after %ss. Closing socket." time-seconds))
                      (.serverClose ch 1013)) ; "Try again later"
        (do (log/error e "Websocket handler failure." data)
            (.serverClose ch 1011) ; "Server process crash"
            )))))

(defn write-msg
  "Return a task, writing a message on a websocket when run."
  [^AsyncChannel ch message]
  (fn [s f]
    (if (http-kit/send! ch {:body message})
      (s :ack)
      (f (ex-info "Can't send message to client, remote channel is closed" {})))
    #()))

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

(defn handle-electric-ws [request handler-f]
  (let [state             (object-array 2)
        on-message-slot   (int 0)
        on-close-slot     (int 1)
        keepalive-mailbox (m/mbx)]
    {:init (fn on-connect [^AsyncChannel ch]
             (aset state on-close-slot
               ((m/join (fn [& _])
                  (timeout keepalive-mailbox ELECTRIC-CONNECTION-TIMEOUT)
                  (handler-f (partial write-msg ch) (r/subject-at state on-message-slot))
                  (send-hf-heartbeat ELECTRIC-HEARTBEAT-INTERVAL #(http-kit/send! ch %)))
                 {} (partial failure ch))))  ; Start Electric process
     :on-close   (fn on-close [^AsyncChannel ch status]
                   (case status ; https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4.1
                     :server-close       nil
                     :normal             (prn "Client disconnected gracefully" status)
                     :going-away         (prn "Client navigated away" status)
                     ;; 1005 is the default close code set by Chrome and FF unless specified.
                     :no-status-received (prn "Client disconnected for an unknown reason (browser default close code)" status)
                     (prn "Client disconnected for an unexpected reason." status))
                   ((aget state on-close-slot)))
     :on-ping    (fn on-ping [ch data] ; Pong automatically sent by HttpKit. Browsers don't ping.
                   (keepalive-mailbox nil))
     :on-receive (fn on-receive [^AsyncChannel ch text-or-buff]
                   (when-not (= "HEARTBEAT" text-or-buff)
                     ((aget state on-message-slot)
                      (if (string? text-or-buff)
                        text-or-buff
                        (ByteBuffer/wrap text-or-buff))))
                   (keepalive-mailbox nil))}))

(defn electric-ws-message-handler
  "Given a ring request, a writer task function and a subject emitting messages, run an Electric
  program named by the client. Original HTTP upgrade ring request map is
  accessible using `(ring.adapter.jetty9/req-of ws)`."
  [ring-req entrypoint write-msg read-msg]
  ((entrypoint ring-req) (comp write-msg io/encode) (fn [cb] (read-msg (comp cb io/decode)))))

(defn reject-websocket-handler
  "Will accept socket connection upgrade and immediately close the socket on
  connection, with given `code`. Use this to cleanly reject a
  websocket connection."
  ;; Rejecting the HTTP 101 Upgrade request would also prevent the socket to
  ;; open, but for security reasons, the client is never informed of the HTTP
  ;; 101 failure cause.
  [code]
  {:on-open (fn [^AsyncChannel ch] (.serverClose ch code))})
