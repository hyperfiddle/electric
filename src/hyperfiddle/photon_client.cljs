(ns hyperfiddle.photon-client
  (:require [contrib.cljs-target :refer [do-browser]]
            [missionary.core :as m]
            [hyperfiddle.photon.impl.runtime :as r]
            [hyperfiddle.photon.impl.io :as io])
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

(def retry-codes #{1006 1005}) ; https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1

(defn boot-with-retry [client conn]
  (m/sp
    (loop [delays retry-delays]
      (let [s (object-array 1)]
        (.log js/console "Connecting...")
        (when-some [[delay & delays]
                    (when-some [info (m/? (conn (fn [x] ((aget s 0) x))
                                            (m/ap
                                              (.log js/console "Connected.")
                                              (let [r (m/rdv)]
                                                (m/amb=
                                                  (do (m/? (client r (r/subject-at s 0)))
                                                      (m/amb))
                                                  (loop []
                                                    (if-some [x (m/? r)]
                                                      (m/amb x (recur))
                                                      (m/amb))))))))]
                      (if-some [code (:code info)]
                        (if (contains? retry-codes code)
                          (do (.log js/console "Connection lost.") (seq retry-delays))
                          (throw (ex-info (str "Remote error - " code " " (:reason info)) {})))
                        (do (.log js/console "Failed to connect.") delays)))]
          (.log js/console (str "Next attempt in " (/ delay 1000) " seconds."))
          (recur (m/? (m/sleep delay delays))))))))