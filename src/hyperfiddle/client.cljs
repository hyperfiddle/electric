(ns hyperfiddle.client
  (:require [missionary.core :as m]
            [clojure.edn :as edn]
            [hyperfiddle.common.transit :as transit]
            [hyperfiddle.client.ui :as ui]
            [hyperfiddle.client.edn-view :as ev]
            [hfdl.lang :as d])
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
        (.send ws (transit/encode x))
        (s nil)
        (catch :default e
          (f e)))
      #())))

(defn reader [ws]
  (m/observe
    (fn [!]
      (set! (.-onclose ws) (fn [x] (js/console.log x)))
      (set! (.-onmessage ws) (fn [x] (! (transit/decode (.-data x)))))
      #(set! (.-onmessage ws) nil))))

(def env (merge d/exports ui/exports ev/exports (d/vars prn pr-str m/watch atom hf/->Input)))

(def ^:export main
  (m/sp
    (let [ws (m/? connect)]
      (m/? (d/peer env #() (writer ws) (reader ws))))))
