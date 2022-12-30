(ns hyperfiddle.photon-client
  (:require [contrib.cljs-target :refer [do-browser]]
            [hyperfiddle.logger :as log]
            [missionary.core :as m]
            [hyperfiddle.photon-impl.io :as io])
  (:import missionary.Cancelled))

(do-browser
  (defn server-url []
    (let [proto (.. js/window -location -protocol)]
      (str (case proto
             "http:" "ws:"
             "https:" "wss:"
             (throw (ex-info "Unexpected protocol" proto)))
           "//"
           (.. js/window -location -host)))))

(defn connect! [socket]
  (let [deliver (m/dfv)]
    (log/info "WS Connecting …")
    (set! (.-binaryType socket) "arraybuffer")
    (doto socket
      (.addEventListener "open" (fn [] (log/info "WS Connected.")
                                  (deliver socket)))
      (.addEventListener "error" (fn on-error [err]
                                   (.removeEventListener socket "error" on-error)
                                   (log/debug "WS Error" err)
                                   (deliver nil))))
    deliver))

(defn wait-for-close [socket]
  (let [ret (m/dfv)]
    (.addEventListener socket "close" (fn [^js event] (ret [(.-code event) (.-reason event)])))
    ret))

(defn make-write-chan [socket mailbox]
  (.addEventListener socket "message" #(mailbox (.-data %)))
  (fn
    ([] (.close socket 1000 "graceful shutdown"))
    ([value]
     (fn [success failure]
       (try
         (log/trace "🔼" value)
         (.send socket value)
         (success nil)
         (catch :default e
           (log/error "Failed to write on socket" e)
           (failure e)))
       #(log/info "Canceling websocket write")))))

(defn connect [mailbox ws-server-url]
  (m/ap (loop [] (if-let [socket (m/? (connect! (new js/WebSocket ws-server-url)))]
                   (m/amb (do (some-> js/document (.getElementById "root") (.setAttribute "data-ws-connected" "true"))
                            (make-write-chan socket mailbox))
                     (do (log/info "WS Waiting for close…")
                       (when (let [[code reason] (m/? (wait-for-close socket))]
                               (case code
                                 1011 (do (log/error "WS closed" code reason)
                                          false)
                                 true))
                         (some-> js/document (.getElementById "root") (.setAttribute "data-ws-connected" "false"))
                         (log/info "WS Closed.")
                         (log/info "WS Retry in 2 seconds…")
                         (m/? (m/sleep 2000))
                         (recur))))
                   (do (log/info "WS Failed to connect, retrying in 2 seconds…")
                     (some-> js/document (.getElementById "root") (.setAttribute "data-ws-connected" "false"))
                     (m/? (m/sleep 2000))
                     (recur))))))

(def ^:dynamic *ws-server-url* (do-browser (server-url)))

(defn client [client server]
  (m/reduce {} nil
    (m/ap
      (let [mailbox (m/mbx)]
        (if-let [write-chan (m/?< (connect mailbox *ws-server-url*))]
          (try (log/info "Starting Photon Client")
               (m/? (write-chan (io/encode server)))        ; bootstrap server
               (m/? (client (io/message-writer write-chan)
                      (io/message-reader mailbox)))
               (finally (write-chan)))
          (log/info "Stopping Photon Client"))))))
