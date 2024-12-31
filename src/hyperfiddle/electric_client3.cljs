(ns hyperfiddle.electric-client3
  (:require [contrib.cljs-target :refer [do-browser]]
            [missionary.core :as m]
            [hyperfiddle.electric.impl.runtime3 :as r])
  (:import missionary.Cancelled))

(goog-define ELECTRIC_USER_VERSION "hyperfiddle_electric_client__dirty") ; url safe

(do-browser
  (defn server-url []
    (let [url (new js/URL (.-location js/window))
          proto (.-protocol url)]
      (set! (.-protocol url)
        (case proto
          "http:" "ws:"
          "https:" "wss:"
          (throw (ex-info "Unexpected protocol" proto))))
      (.. url -searchParams (set "ELECTRIC_USER_VERSION" ELECTRIC_USER_VERSION))
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
    #_ ; todo tune perf
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
  (m/reduce {} nil (m/ap (m/? (wait-for-flush (send! ws (m/?> msgs)))))))

(defn handle-hf-heartbeat [ws cb]
  (fn [msg]
    (if (= msg "HEARTBEAT")
      (send! ws "HEARTBEAT")
      (cb msg))))

(defn fib-iter [[a b]]
  (case b
    0 [1 1]
    [b (+ a b)]))

(def fib (map first (iterate fib-iter [1 1])))

(comment (take 5 fib2) := [1 1 2 3 5])

(def retry-delays (map (partial * 1000) (next fib)))
;; Browsers throttle websocket connects after too many attempts in a short time.
;; To prevent using browsers as port scanners.
;; Symptom: WS takes a long time to establish a connection for no apparent reason.
;; Sometimes happens in dev after multiple page refreshes in a short time.

(comment (take 5 retry-delays))

(defn wait-for-window-to-be-visible
  "Return a task completing when the current browser tab or window becomes visible
  to the user, or immediately if it is already visible. Use case: detect when a
  background tab becomes active again."
  []
  (let [visible! (m/dfv)
        visible? #(= "visible" (.-visibilityState js/document))]
    (letfn [(on-visibility-change [_]
              ;; don't use a one-off event-listener because the visiblitichange
              ;; event's spec doesn't say "visible" means the page was "hidden"
              ;; before. "hidden" or "visible" could therefore fire more than
              ;; once. Spec: https://html.spec.whatwg.org/multipage/interaction.html#page-visibility
              (when (visible?)
                (.removeEventListener js/document "visibilitychange" on-visibility-change)
                (visible! true)))]
      (if (visible?)
        (visible! true)
        (.addEventListener js/document "visibilitychange" on-visibility-change)))
    visible!))

(defn connector [url]
  (let [state (object-array 1)]
    [(fn [cb]
       (let [ws (aget state 0)]
         (set! (.-onmessage ws) (comp (handle-hf-heartbeat ws cb) payload))
         #(set! (.-onmessage ws) nil)))
     (fn [events]
       (m/sp
         (loop [delays retry-delays]
           (.log js/console "Connecting...")
           (when-some [[delay & delays]
                       (if-some [ws (m/? (connect url))]
                         (when-some [{:keys [code] :as info}
                                     (try
                                       (aset state 0 ws)
                                       (m/? (m/join {} (send-all ws events) (wait-for-close ws)))
                                       (finally
                                         (aset state 0 nil)
                                         (when-not (= (.-CLOSED js/WebSocket) (.-readyState ws))
                                           (.close ws) (m/? (m/compel (wait-for-close ws))))))]
                           (when (case code ; https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1
                                   (1000 1001) (do (js/console.debug (str "Electric websocket disconnected - " code)) true)
                                   ;; (1002) ; WS protocol error
                                   ;; (1003) ; Invalid data format (e.g. text vs binary)
                                   ;; (1004) ; Reserved by WS spec, might be defined in the future
                                   (1005 1006) (do (js/console.log "Electric Websocket connection lost.") true)
                                   ;; (1007) ; Inconsistent data in message (e.g. non-UTF8 in text message)
                                   (1008) (throw (ex-info "Stale Electric client" {:hyperfiddle.electric/type ::stale-client}))
                                   ;; (1009) ; Message to big
                                   ;; (1010) ; Couldn't negotiate WS extension with server - client terminated connection
                                   (1011) ; Server crash, do not attempt to reconnect, let the user decide.
                                   (js/console.log "Electric server terminated unexpectedly - " (pr-str info))
                                   (1012) ; Not defined by WS spec - We use it for Incompatible Client. Do not attempt to reconnect (it would fail again).
                                   (js/console.error (str "A mismatch between Electric client and server's programs was detected."
                                                       "\nThe connection was closed. Refresh the page to attempt a reconnect."
                                                       "\nCommonly, in local dev envs, this is a stale browser tab auto-reconnecting, or the clj and cljs REPLs are out of sync due to evaluating an Electric def in one process but not the other."
                                                       "\nThis should not happen in prod. See `https://github.com/hyperfiddle/electric-starter-app/` for a reference configuration."))
                                   (1013) ; Not defined by WS spec - We use it for Server Timeout. i.e. "Server closed the connection because it didn't hear of this client for too long".
                                   (do (js/console.log "Electric server timed out, considering this Electric client inactive.")
                                       true)
                                   ;; (1014) ; Not defined by WS spec.
                                   ;; (1015) ; Reserved by WS spec - used for failed TLS handshakes - do not overload.
                                   ;; else
                                   (do (js/console.log (str "Electric Websocket disconnected for an unexpected reason - " (pr-str info)))
                                       true))
                             (m/? (wait-for-window-to-be-visible))
                             delays))
                         (do (.log js/console "Electric client failed to connect to Electric server.") delays))]
             (.log js/console (str "Next attempt in " (/ delay 1000) " seconds."))
             (recur (m/? (m/sleep delay delays)))))))]))

(defn reload-when-stale [task]
  (fn [s f]
    (task s (fn [error]
              (when (= ::stale-client (:hyperfiddle.electric/type (ex-data error)))
                (do (js/console.log "Electric server and Electric client version mismatches. Refreshing page to load new assets.")
                  (.reload (.-location js/window))))
              (f error)))))
