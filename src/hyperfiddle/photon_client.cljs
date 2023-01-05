(ns hyperfiddle.photon-client
  (:require [contrib.cljs-target :refer [do-browser]]
            [missionary.core :as m]
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.photon-impl.io :as io])
  (:import missionary.Cancelled))

(do-browser
  (defn server-url []
    (let [proto (.. js/window -location -protocol)]
      (str (case proto
             "http:" "ws:"
             "https:" "wss:"
             (throw (ex-info "Unexpected protocol" proto)))
           "//"
           (.. js/window -location -host)))))

(def ^:dynamic *ws-server-url* (do-browser (server-url)))

(defn remove-listeners [ws]
  (set! (.-onopen ws) nil)
  (set! (.-onclose ws) nil))

(defn connect [url]
  (fn [s f]
    (try
      (let [ws (new js/WebSocket url)]
        (set! (.-binaryType ws) "arraybuffer")
        (set! (.-onopen ws)
          (fn [_]
            (remove-listeners ws)
            (s ws)))
        (set! (.-onclose ws)
          (fn [e]
            (remove-listeners ws)
            (f (ex-info "Failed to connect."
                 {:code (.-code e)
                  :reason (.-reason e)}))))
        #(when (= (.-CONNECTING js/WebSocket) (.-readyState ws))
           (.close ws)))
      (catch :default e
        (f e) #()))))

(defn wait-for-flush [ws]
  (m/sp
    (while (pos? (.-bufferedAmount ws))
      (m/? (m/sleep 50)))))

(defn wait-for-close [ws]
  (fn [s f]
    (set! (.-onclose ws)
      (fn [e]
        (set! (.-onclose ws) nil)
        (s {:code (.-code e)
            :reason (.-reason e)})))
    #(do (set! (.-onclose ws) nil)
         (f (Cancelled.)))))

(defn send! [ws msg]
  (doto ws (.send msg)))

(defn send-all [ws msgs]
  (m/reduce {} nil (m/ap (m/? (wait-for-flush ((io/encoder send!) ws (m/?> msgs)))))))

(def decode-message-data
  (comp (map #(.-data %)) io/decoder))

(defn connector "
server : the server part of the program
cb : the callback for incoming messages.
msgs : the discrete flow of messages to send, spawned when websocket is connected, cancelled on websocket close.
Returns a task producing nil or failing if the websocket was closed before end of reduction.
" [server]
  (fn [cb msgs]
    (m/sp
      (let [ws (m/? (connect *ws-server-url*))]
        (try
          (send! ws (io/encode server))
          (set! (.-onmessage ws) (partial (decode-message-data io/foreach) cb))
          (when-some [close-info (m/? (m/race (send-all ws msgs) (wait-for-close ws)))]
            (throw (ex-info "Connection lost." close-info)))
          (finally
            (when-not (= (.-CLOSED js/WebSocket) (.-readyState ws))
              (.close ws) (m/? (m/compel wait-for-close)))))))))

(def retry-delays (iterate (partial * 2) 500))

(defn boot-with-retry [client conn]
  (m/sp
    (loop [delays retry-delays]
      (let [s (object-array 1)]
        (.log js/console "Connecting...")
        (when-some [[delay & delays]
                    (try (m/? (conn (fn [x] ((aget s 0) x))
                                (m/ap
                                  (.log js/console "Connected.")
                                  (let [r (m/rdv)]
                                    (m/amb=
                                      (do (m/? (client r (r/subject-at s 0)))
                                          (m/amb))
                                      (loop []
                                        (if-some [x (m/? r)]
                                          (m/amb x (recur))
                                          (m/amb))))))))
                         (catch ExceptionInfo e
                           (.log js/console (ex-message e))
                           (case (:code (ex-data e))
                             1006 delays retry-delays)))]
          (.log js/console (str "Next attempt in " (/ delay 1000) " seconds."))
          (recur (m/? (m/sleep delay delays))))))))