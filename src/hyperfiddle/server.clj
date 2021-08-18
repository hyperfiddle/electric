(ns hyperfiddle.server
  (:require
    [hyperfiddle.common.transit :as transit]
    [hyperfiddle.server.websockets :as ws]                  ;; TODO restore
    [hyperfiddle.server.interceptors :as i]
    hyperfiddle.server.logging
    [hyperfiddle.server.routes :as routes]
    [io.pedestal.http :as http]
    [io.pedestal.http.ring-middlewares :as middlewares]
    [io.pedestal.http.route :refer [expand-routes]]
    [io.pedestal.http.secure-headers :as secure-headers]
    [io.pedestal.interceptor.helpers :refer [before]]
    [ring.middleware.file :as file]
    [taoensso.timbre :as log]
    [missionary.core :as m]
    [hfdl.impl.util :as u]
    [hfdl.lang :as p]
    [hyperfiddle.todomvc :as t]
    [hyperfiddle.api :as h]
    [hyperfiddle.photon-dom :as dom])
  (:import org.eclipse.jetty.server.handler.gzip.GzipHandler
           (org.eclipse.jetty.servlet ServletContextHandler)))

(def base-config {::http/type            :jetty
                  ::http/allowed-origins {:allowed-origins (constantly true)}
                  ::http/secure-headers  {:content-security-policy-settings (secure-headers/content-security-policy-header {:object-src "'none'"})
                                          :content-type-settings            (secure-headers/content-type-header)}
                  ::http/join?           false})

(def interceptors [i/trace #_i/cookies i/negociate i/auto-content-type #_i/body-params])

(def rewrite-resources-path
  (before (fn [{:keys [request] :as context}]
            (assoc-in context [:request :path-info] (str "/" (get-in request [:path-params :file-path]))))))


(defn set-path [request path]
  (assoc request :uri path, :path-info path))

(def !req (atom nil))
(comment
  (-> @!req
      (set-path "/edn_view/edn_view.html")
      (file/file-request "resources/public")))

(def index-dispatch
  (before (fn index-dispatch
            [{:keys [request] :as context}]
            (prn request)
            (reset! !req request)
            (assoc context :response
                   (let [res (file/file-request (set-path request "/index.html")
                                                "resources/public")]
                     #_(-> (assoc-in res [:headers "Content-Type"] "text/html")
                         (update :body slurp))
                     res)))))

(defn edn->str [x]
  (m/via m/cpu #_(pr-str x)
         (try (transit/encode x)
              (catch Throwable t
                (log/error (ex-info "Failed to encode" {:value x} t))
                (throw t)))))

(defn str->edn [x]
  (m/via m/cpu #_(edn/read-string x)
         (try (transit/decode x)
              (catch Throwable t
                (log/error (ex-info "Failed to decode" {:value x} t))
                (throw t)))))

(defn edn-reader [x]
  (m/ap (m/? (str->edn (m/? (m/?> (m/seed (repeat x))))))))

(defn edn-writer [remote]
  (fn [x] (m/sp (m/? (ws/write-str remote (m/? (edn->str x)))))))

(defn start! [task]
  (task prn u/pst))

(def env (merge p/exports dom/exports t/exports h/exports))

;; to boot a distributed photon app
;; p/main is expanded on cljs side, returns a pair
;; first element is the compiled client version
;; second element is the server version as data.
;; When client starts, it initializes the ws connection, then sends the server program as first message, the boots its
;; local reactor.
;; The server listens to incoming ws connections, waits for the program as first message, the evaluates the program and
;; boots its local reactor.

(defn ws-paths [_config]
  {"/echo" (fn [_request]
             (fn [remote read-str read-buf closed]
               (start! (m/sp (loop [] (m/? (ws/write-str remote (m/? read-str))) (recur))))))
   "/ws"   (fn [_request]
             (fn [remote read-str read-buf closed]
               (start!
                 (m/sp
                   (let [program (m/? (str->edn (m/? read-str)))
                         _ (prn :got-program program)
                         bootfn (p/eval env program)]
                     (prn :booting-reactor)
                     (m/? (bootfn (edn-writer remote) (edn-reader read-str))))))))})

(defn gzip-handler [& methods]
  (doto (GzipHandler.) (.addIncludedMethods (into-array methods))))

(defn build [{:keys [host port] :as config}]
  (let [base-chain (conj interceptors (i/with-config config))]
    (merge base-config
      {::http/routes            (->> (expand-routes
                                       #{["/assets/*file-path" :get [rewrite-resources-path (middlewares/file-info) (middlewares/file "resources/public")] :route-name :serve]
                                         ["/*sexpr" :get (conj base-chain (middlewares/file-info) index-dispatch) :route-name :index #_#_:constraints {:sexpr #"(?!ws$).*$"}]})
                                  (routes/sort-by-position :route-name [:serve :index]))

       ::http/router            :linear-search ;; it’s fine we’ve got 1 or 2 routes
       ::http/host              host
       ::http/port              port
       ::http/request-logger    nil            ;; handled by i/trace
       ::http/container-options {:context-configurator
                                 (fn [^ServletContextHandler ctx]
                                   (doto ctx
                                     (.setGzipHandler (gzip-handler "GET" "POST"))
                                     (ws/add-ws-endpoints (ws-paths config))))}})))

(defn start-server! [config]
  (let [server (-> config build http/create-server http/start)]

    (log/info "server started at"
              (str (:scheme config) "://" (:host config) ":" (:port config)))

    ;; TODO re-enable once config is settled
    #_(if-not (= "dev" (System/getenv "mode"))
      (.join (::http/server server))
      server)
    server))

(def default-config {:host   "localhost"
                     :port   8080
                     :scheme "http"})


(comment
  (def server (start-server! default-config))
  (http/stop server)
  )
