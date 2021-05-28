(ns hyperfiddle.client
  (:require [missionary.core :as m]
            [hfdl.lang :as d]
            [hyperfiddle.common.transit :as transit]
            [dustin.fiddle-pages :as f]
            [hyperfiddle.common.routes :as common-routes]
            [hyperfiddle.client.ui :as ui]
            [hyperfiddle.client.edn-view :as ev])
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

(defn client [app]
  (m/sp
    (let [ws (m/? connect)]
      (m/? (d/client app (writer ws) (reader ws))))))

(def sampler)

(defn define-sampler! [s]
  (js/console.log (case s nil "booting..." "operational."))
  (set! sampler s))

(def ^:export main
  (client
    (d/boot define-sampler! #_f/ui-view
      (d/dataflow
        (let [route-request @common-routes/>route]
          (ev/set-editor-value! (ev/editor (ui/by-id "hf-edn-view-route") ui/change-route!) ~@route-request)))
      )))