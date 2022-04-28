(ns hyperfiddle.client
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.common.transit :as transit]
            [hyperfiddle.dev.logger :as log]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            ["reconnecting-websocket" :as ReconnectingWebSocket]
            user.browser
            user.hytradboi
            user.orders-ui
            hyperfiddle.ui ; hot-reload p/defs on save
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

(def main
  (client
    (p/main
      (binding [dom/parent (dom/by-id "hf-ui-dev-root")]
        (dom/div
         (dom/attribute "id" "main")
         (dom/class "browser")
         (dom/div
          (dom/class "view")
          (new user.browser/View)
          #_~user.hytradboi/view
          #_(new user.orders-ui/Orders)))))))

(def ^:export reactor)

(defn ^:dev/before-load stop! []
  (if reactor
    (do (log/info "Stopping reactor…")
        (reactor) ;; dispose
        (set! reactor nil)
        (log/info "Reactor stopped"))
    (log/info "Reactor already stopped")))

(defn ^:dev/after-load ^:export start! []
  (if-not reactor
    (do (log/info "Starting reactor…")
        (set! reactor (main js/console.log #(do (log/error "Uncaugh error in main process" %)
                                                #_(stop!))))
        (log/info "Reactor started."))
    (log/info "Reactor already started") ))

