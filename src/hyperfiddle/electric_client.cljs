(ns hyperfiddle.electric-client
  (:require [contrib.cljs-target :refer [do-browser]]
            [missionary.core :as m]
            [hyperfiddle.electric.impl.runtime :as r]
            [hyperfiddle.electric.impl.io :as io])
  (:import missionary.Cancelled))

(goog-define VERSION "")

(do-browser
  (defn server-url []
    (let [url (new js/URL (.-location js/window))
          proto (.-protocol url)]
      (set! (.-protocol url)
        (case proto
          "http:" "ws:"
          "https:" "wss:"
          (throw (ex-info "Unexpected protocol" proto))))
      (.. url -searchParams (set "HYPERFIDDLE_ELECTRIC_CLIENT_VERSION" VERSION))
      (set! (.-hash url) "") ; fragment is forbidden in WS URL https://websockets.spec.whatwg.org/#ref-for-dom-websocket-websocket%E2%91%A0
      (.toString url))))

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
          (fn [_]
            (remove-listeners ws)
            (s nil)))
        #(when (= (.-CONNECTING js/WebSocket) (.-readyState ws))
           (.close ws)))
      (catch :default e
        (f e) #()))))

(defn wait-for-flush [ws]
  (m/sp
    (while (< 4096 (.-bufferedAmount ws))
      (m/? (m/sleep 50)))))

(defn wait-for-close [ws]
  (fn [s f]
    (set! (.-onclose ws)
      (fn [e]
        (set! (.-onclose ws) nil)
        (s {:code (.-code e)
            :reason (.-reason e)})))
    #(when-not (nil? (.-onclose ws))
       (set! (.-onclose ws) nil)
       (f (Cancelled.)))))

(defn payload [x]
  (.-data x))

(defn send! [ws msg]
  (doto ws (.send msg)))

(defn send-all [ws msgs]
  (m/reduce {} nil (m/ap (m/? (wait-for-flush (send! ws (io/encode (m/?> msgs))))))))

(defn connector "
server : the server part of the program
cb : the callback for incoming messages.
msgs : the discrete flow of messages to send, spawned when websocket is connected, cancelled on websocket close.
Returns a task producing nil or failing if the websocket was closed before end of reduction.
" [server]
  (fn [cb msgs]
    (m/sp
      (if-some [ws (m/? (connect *ws-server-url*))]
        (try
          (send! ws (io/encode server))
          (set! (.-onmessage ws) (comp cb io/decode payload))
          (m/? (m/race (send-all ws msgs) (wait-for-close ws)))
          (finally
            (when-not (= (.-CLOSED js/WebSocket) (.-readyState ws))
              (.close ws) (m/? (m/compel wait-for-close)))))
        {}))))

(defn fib-iter [[a b]]
  (case b
    0 [1 1]
    [b (+ a b)]))

(def fib (map first (iterate fib-iter [1 1])))

(comment (take 5 fib2) := [1 1 2 3 5])

(def retry-delays (map (partial * 100) fib))

(comment (take 5 retry-delays))

(defn boot-with-retry [client conn]
  (m/sp
    (let [ws-server-url *ws-server-url*]
      (loop [delays retry-delays]
        (let [s (object-array 1)]
          (.log js/console "Connecting...")
          (when-some [[delay & delays]
                      (when-some [info (binding [*ws-server-url* ws-server-url]
                                         (m/? (conn (fn [x] ((aget s 0) x))
                                                (m/ap
                                                  (.log js/console "Connected.")
                                                  (let [r (m/rdv)]
                                                    (m/amb=
                                                      (do (m/? (client r (r/subject-at s 0)))
                                                          (m/amb))
                                                      (loop []
                                                        (if-some [x (m/? r)]
                                                          (m/amb x (recur))
                                                          (m/amb)))))))))]
                        (if-some [code (:code info)]
                          (case code ; https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1
                            (1005 1006) (do (.log js/console "Connection lost.") (seq retry-delays))
                            (1008) (throw (ex-info "Stale client" {:hyperfiddle.electric/type ::stale-client}))
                            (throw (ex-info (str "Remote error - " code " " (:reason info)) {})))
                          (do (.log js/console "Failed to connect.") delays)))]
            (.log js/console (str "Next attempt in " (/ delay 1000) " seconds."))
            (recur (m/? (m/sleep delay delays)))))))))

(defn reload-when-stale [task]
  (fn [s f]
    (task s (fn [error]
              (when (= ::stale-client (:hyperfiddle.electric/type (ex-data error)))
                (do (js/console.log "Server and client version mismatch. Refreshing page.")
                  (.reload (.-location js/window))))
              (f error)))))