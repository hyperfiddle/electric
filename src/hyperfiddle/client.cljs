(ns hyperfiddle.client
  (:require [missionary.core :as m]
            [hyperfiddle.common.transit :as transit]
            [hyperfiddle.todomvc :as t]
            [hyperfiddle.api :as h]
            [dev]
            [hyperfiddle.photon-dom :as dom]
            [hfdl.lang :as p])
  (:require-macros))

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

(def root (js/document.getElementById "hf-ui-dev-root"))

(def ^:export main
  (client (p/main #_(p/binding [dom/parent root] ~t/app)
            (dom/set-text-content! root (str "hello " ~@~(m/watch h/info) " !")))))