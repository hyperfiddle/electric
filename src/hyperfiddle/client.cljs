(ns hyperfiddle.client
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.common.transit :as transit]
            [hyperfiddle.dev.logger :as log]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            user.browser
            user.hytradboi)
  (:require-macros [hyperfiddle.ui6] ;; hot-reload p/defs on save
                   ))

;; TODO reconnect on failures
(defn connect [cb]
  (fn [s f]
    (let [socket (new js/WebSocket (str "ws://" (.. js/document -location -host) "/ws"))]
      (set! (.-onopen socket)
        (fn [_]
          (set! (.-onopen socket) nil)
          (set! (.-onerror socket) nil)
          (set! (.-onclose socket) js/console.log)
          (set! (.-onmessage socket)
            #(let [decoded (transit/decode (.-data %))]
               (log/trace "🔽" decoded)
               (cb decoded)))
          (s (fn [x]
               (fn [s f]
                 (try
                   (log/trace "🔼" x)
                   (.send socket (transit/encode x))
                   (s nil)
                   (catch :default e
                     (log/error e)
                     (f e)))
                 #())))))
      (set! (.-onerror socket)
        (fn [err]
          (set! (.-onopen socket) nil)
          (set! (.-onerror socket) nil)
          (f err))))
    #(prn :TODO-CANCEL-WS-CONNECT)))

(defn client [[c s]]
  (m/sp
    (let [m (m/mbx)
          w (m/? (connect m))]
      (m/? (w s))
      (m/? (c w m)))))

(def main
  (client
    (p/main
      (binding [dom/parent (dom/by-id "hf-ui-dev-root")]
        ~@;; server
            (binding [hf/db (hf/->DB "$" 0 nil hf/*$*)]
              ~@;; client
                  (dom/div
                    (dom/class "browser")
                    (dom/div
                      (dom/class "view")
                      ~user.browser/view
                      #_~user.hytradboi/view)))))))

(def ^:export reactor)

(defn ^:dev/after-load ^:export start! []
  (if-not reactor
    (do (log/info "Starting reactor…")
        (set! reactor (main js/console.log js/console.error))
        (log/info "Reactor started."))
    (log/info "Reactor already started") ))

(defn ^:dev/before-load stop! []
  (if reactor
    (do (log/info "Stopping reactor…")
        (reactor) ;; dispose
        (set! reactor nil)
        (log/info "Reactor stopped"))
    (log/info "Reactor already stopped")))

