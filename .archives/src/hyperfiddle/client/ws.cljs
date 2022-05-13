(ns hyperfiddle.client.ws
  (:require ;; [bidi.bidi :as bidi]
            ;; [hypercrud.transit :as hc-t]
            [hyperfiddle.api :as hf]
            ;; [hyperfiddle.io.client.http :as http]
            ;; [hyperfiddle.service.routes :as routes]
            [cljs.reader :as edn] ;; TODO replace with transit
            [promesa.core :as p]
            [hyperfiddle.logger :as log]))

(def READY_STATE {0 ::connecting
                  1 ::open
                  2 ::closing
                  3 ::closed})

(defonce ^:private a-socket (atom nil))
(defonce ^:private state (atom {}))
(defonce ^:private connecting (atom nil))

(declare on-message)

(defn- get-token! []
  ;; (http/request! {:method  :get
  ;;                 :url     (str (.. js/document -location -origin) (bidi/path-for routes/routes ::hf/ws-exchange-token))
  ;;                 :headers {"accept" "text/plain"}})
  (p/resolved nil))

(defn- connect! []
  (-> (get-token!)
      (p/then (fn [token]
                (let [prom (p/create (fn [resolve reject]
                                       (try
                                         (let [socket (new js/WebSocket (str "ws://" (.. js/document -location -host) "/ws?token=" token))]
                                           (set! (.-onopen socket) (fn [_event]
                                                                     (reset! connecting false)
                                                                     (resolve socket)))
                                           (set! (.-onmessage socket) on-message)
                                           (set! (.-onerror socket) reject))
                                         (catch js/Error e
                                           (log/error e)
                                           (reject e)))))]
                  (reset! connecting prom)
                  prom)))))

(defn- ready-state [^js socket]
  (get READY_STATE (.-readyState socket)))

(defn- opened? [^js socket]
  (= ::open (ready-state socket)))

(defn- get-client! []
  (p/create (fn [resolve reject]
              (let [connecting @connecting
                    socket     @a-socket
                    next       (fn [socket]
                                 (reset! a-socket socket)
                                 (resolve socket))]
                (cond
                  (and socket (opened? socket)) (resolve socket)
                  connecting                    (p/then connecting next)
                  :else                         (-> (connect!)
                                                    (p/then next)
                                                    (p/catch reject)))))))

(defn- disconnect! []
  (when-let [^js socket @a-socket]
    (when (opened? socket)
      (.close socket))))

(defn send! [event]
  (let [continuation (p/deferred)]
    (-> (get-client!)
        (p/then (fn [^js socket]
                  (let [id (random-uuid)]
                    (swap! state assoc id continuation)
                    (.send socket (pr-str #_hc-t/encode (assoc event :id id))))))
        (p/catch (fn [err]
                   (log/error "Unable to get a socket" err)
                   (p/resolve! continuation nil))))
    continuation))

(defn- on-message [^js message]
  (let [{:keys [id type] :as data} (edn/read-string #_hc-t/decode (.-data message))]
    (swap! state (fn [state]
                   (if-let [continuation (get state id)]
                     (do (case type
                           :error (p/reject! continuation data)
                           (p/resolve! continuation data))
                         (dissoc state id))
                     (do (log/info "No continuation for this answer" data)
                         state))))))
