(ns hyperfiddle.client
  (:require [dev]
            [hfdl.lang :as p]
            [hyperfiddle.common.transit :as transit]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [hyperfiddle.router :as r]
            [user.hfql-distributed :as distributed]
            [hyperfiddle.dev.logger :as log]))

(def ^:export LATENCY 0)

(defn delayed [task]
  (m/sp (m/? (m/sleep (/ LATENCY 2))) (m/? task)))

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
      (m/? (c (comp delayed w) (delayed m))))))

(def ^:export main
  (client (p/main (binding [dom/parent (dom/by-id "hf-ui-dev-root")] ~distributed/page #_~r/router))))

