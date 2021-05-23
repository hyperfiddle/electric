(ns hyperfiddle.client
  (:require [missionary.core :as m]
            [hyperfiddle.common.transit :as transit]
            [hyperfiddle.client.ui :as ui]
            [hyperfiddle.client.edn-view :as ev]
            [hfdl.lang :as d]
            [hyperfiddle.api :as hf]
            [hyperfiddle.client.ui.demo]
            [hyperfiddle.common.routes :as routes])
  (:require-macros [hfdl.lang :as d]))

;; TODO reconnect on failures
(def connect
  (fn [s f]
    (let [socket (new js/WebSocket (str "ws://" (.. js/document -location -host) "/ws"))
          clean! (fn []
                   (set! (.-onerror socket) nil)
                   (set! (.-onopen socket) nil))]
      (set! (.-onopen socket) (fn [_] (clean!) (s socket)))
      (set! (.-onerror socket) (fn [err] (clean!) (f err))))
    #(prn :TODO-CANCEL-WS-CONNECT)))

(defn writer [ws]
  (fn [x]
    (fn [s f]
      (try
        (js/console.log "🔼" x)
        (.send ws (transit/encode x))
        (s nil)
        (catch :default e
          (js/console.error e)
          (f e)))
      #())))

(defn reader [ws]
  (m/observe
    (fn [!]
      (set! (.-onclose ws) (fn [x] (js/console.log x)))
      (set! (.-onmessage ws) (fn [x] (! (let [decoded (transit/decode (.-data x))]
                                         (js/console.log "🔽" decoded)
                                         decoded))))
      #(set! (.-onmessage ws) nil))))

(def env (merge d/exports ui/exports ev/exports (d/vars prn pr-str m/watch atom hf/->Input routes/>route)))

(def ^:export main
  (m/sp
    (let [ws (m/? connect)]
      (m/? (d/peer env #() (writer ws) (reader ws))))))
