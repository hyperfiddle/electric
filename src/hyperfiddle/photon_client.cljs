(ns hyperfiddle.photon-client
  (:require [triage.transit :as transit]
            [triage.logger :as log]
            [missionary.core :as m]
            ["reconnecting-websocket" :as ReconnectingWebSocket]
            ))

(defn heartbeat! [interval socket]
  (js/setTimeout (fn rec []
                   (when (= 1 (.-readyState socket))
                     (.send socket "heartbeat")
                     (js/setTimeout rec interval)))))

;; TODO reconnect on failures
(defn connect [cb]
  (fn [s f]
    (let [client-id   (random-uuid)
          max-retries 4
          socket      (new ReconnectingWebSocket
                           (str "ws://" (.. js/document -location -host) "/ws?client-id=" client-id)
                           #js[] #js{:maxRetries (inc max-retries)})
          on-open     (fn on-open [_]
                        (heartbeat! 30000 socket)
                        (.removeEventListener socket on-open)
                        (set! (.-onmessage socket)
                              #(let [decoded (transit/decode (.-data %))]
                                 (log/trace "🔽" decoded)
                                 (cb decoded)))
                        (s (fn
                             ([] (.close socket))
                             ([x]
                              (fn [s f]
                                (try
                                  (log/trace "🔼" x)
                                  (.send socket (transit/encode x))
                                  (s nil)
                                  (catch :default e
                                    (log/error "Failed to write on socket" e)
                                    (f e)))
                                #(log/info "Canceling websocket write")))))
                        (some-> js/document (.getElementById "main") (.setAttribute "data-ws-connected" "true")))
          on-close    (fn [socket]
                        (log/debug "WS close" socket)
                        (some-> js/document (.getElementById "main") (.setAttribute "data-ws-connected" "false")))
          on-error    (fn [err]
                        (if (> (.-retryCount socket) max-retries)
                          (when (js/confirm "Socket failed to reconnect, please refresh.")
                            (.close socket)
                            (f err))
                          (log/debug "WS error" err)))]
      (.addEventListener socket "open" on-open)
      (.addEventListener socket "close" on-close)
      (.addEventListener socket "error" on-error)
      #(prn "TODO Cancel connection in connecting state"))))

(defn client [[c s]]
  (m/sp
    (let [m (m/mbx)
          write-chan (m/? (connect m))]
      (try (m/? (write-chan s))
           (m/? (c write-chan m))
           (catch :default err
             (write-chan) ; close channel socket
             (throw err)
             )))))
