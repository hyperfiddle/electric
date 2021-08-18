(ns hyperfiddle.client
  (:require [dev]
            [hfdl.lang :as p]
            [hyperfiddle.common.transit :as transit]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [hyperfiddle.router :as r]))

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

(defn client [[c s]]
  (m/sp
    (let [ws (m/? connect)]
      (m/? ((writer ws) s))
      (m/? (c (writer ws) (reader ws))))))

(def ^:export main
  (client (p/main (p/binding [dom/parent (dom/by-id "hf-ui-dev-root")] ~r/router))))
