(ns hyperfiddle.client
  (:require [missionary.core :as m]
            [hfdl.lang :as d]
            [hyperfiddle.common.transit :as transit]
            [hyperfiddle.common.routes :as common-routes]
            [hyperfiddle.client.ui :as ui]
            [hyperfiddle.client.edn-view :as ev]
            [hyperfiddle.client.ui.demo-dataflow :refer [edn-renderer]]
            [hyperfiddle.client.ui.demo-edn]
            [user.tutorial])
  (:require-macros
    [hfdl.lang :as d]
    [hyperfiddle.client.ui.demo-dataflow
     :refer [echo edn-key-value edn-renderer]]))

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
      (m/? (d/peer c (writer ws) (reader ws))))))

(def ^:export main
  (client
    (d/main
      (let [route-request ~common-routes/>route]
        (ui/mount-component-at-node!
          "hf-ui-dev-root"
          (edn-renderer
            #_(user.tutorial/plus 1 ~@~(m/watch user.tutorial/!input))
            ~@(user.tutorial/render-shirt-size
                ~(m/watch user.tutorial/!shirt-size)))

          #_(edn-renderer {:a 1
                           :b {:c 2}
                           :d :e}))
        #_(ev/set-editor-value!
          (ev/editor (ui/by-id "hf-edn-view-route") ui/change-route!)
          ~@route-request)))))