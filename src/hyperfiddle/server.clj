(ns hyperfiddle.server
  (:require
    [triage.transit :as transit]
    [hyperfiddle.server.websockets :as ws]
    [hyperfiddle.server.interceptors :as i]
    [io.pedestal.http :as http]
    [io.pedestal.http.ring-middlewares :as middlewares]
    [io.pedestal.http.route :refer [expand-routes]]
    [io.pedestal.http.secure-headers :as secure-headers]
    [io.pedestal.interceptor.helpers :refer [before]]
    [ring.middleware.file :as file]
    [triage.logger :as log]
    [missionary.core :as m]
    [hyperfiddle.photon :as p])
  (:import org.eclipse.jetty.server.handler.gzip.GzipHandler
           (org.eclipse.jetty.servlet ServletContextHandler)
           (java.util.concurrent Executors ThreadFactory Executor)
           (missionary Cancelled)))

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
            (log/trace "request" request)
            (reset! !req request)
            (assoc context :response
                   (let [res (file/file-request (set-path request "/index.html")
                                                "resources/public")]
                     #_(-> (assoc-in res [:headers "Content-Type"] "text/html")
                         (update :body slurp))
                     res)))))

(def event-loop
  (let [procs (.availableProcessors (Runtime/getRuntime))
        execs (into []
                (map (fn [i]
                       (Executors/newSingleThreadExecutor
                         (reify ThreadFactory
                           (newThread [_ r]
                             (doto (Thread. r (str "hf-eventloop-" i))
                               (.setDaemon true)))))))
                (range procs))]
    (fn [x] (nth execs (mod (hash x) procs)))))

(defn run-via [^Executor e f]
  (fn [& args]
    (.execute e
      (reify Runnable
        (run [_]
          (apply f args))))))

(defn success [_]
  (println "WS handler completed."))

(defn failure [^Throwable e]
  (.printStackTrace e))

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
             (fn [!remote read-str read-buf closed]
               ((m/sp (try (loop [] (m/? (ws/write-str (deref !remote) (m/? read-str))) (recur))
                           (catch Cancelled _))) success failure)))
   "/ws"   (fn [_request]
             (fn [!remote read-str read-buf closed]
               (let [el (event-loop (deref !remote))]
                 (run-via el
                   ((m/sp
                      (try
                        (let [?read (m/sp
                                      (let [x (try (m/? read-str)
                                                   (finally (m/? (m/via el))))]
                                        (try (transit/decode x)
                                             (catch Throwable t
                                               (log/error (ex-info "Failed to decode" {:value x} t))
                                               (throw t)))))
                              write (fn [x]
                                      (m/sp
                                        (try
                                          (m/? (ws/write-str (deref !remote)
                                                 (try (transit/encode x)
                                                      (catch Throwable t
                                                        (log/error (ex-info "Failed to encode" {:value x} t))
                                                        (throw t)))))
                                          (finally (m/? (m/via el))))))
                              program (m/? ?read)]
                          (prn :booting-reactor #_program)
                          (m/? ((p/eval program) write ?read)))
                        (catch Cancelled _))) success failure)))))})

(defn gzip-handler [& methods]
  (doto (GzipHandler.) (.addIncludedMethods (into-array methods))))

(defn- indices [xs] (into {} (map-indexed (fn [i x] [x i])) xs))

(defn sort-by-position
  "(sort-by-position identity [:a :b :c] [:c :b :a]) => (:a :b :c)"
  ([positions xs]
   (sort-by-position identity positions xs))
  ([getter positions xs]
   (let [index-of (indices positions)]
     (sort (comparator (fn [x y]
                         (let [xi (index-of (getter x))
                               yi (index-of (getter y))]
                           (cond
                             (and xi yi) (< xi yi)
                             xi          true
                             yi          false))))
           xs))))

(defn build [{:keys [host port] :as config}]
  (let [base-chain (conj interceptors (i/with-config config))]
    (merge base-config
      {::http/routes            (->> (expand-routes
                                       #{["/assets/*file-path" :get [rewrite-resources-path (middlewares/file-info) (middlewares/file "resources/public")] :route-name :serve]
                                         ["/*sexpr" :get (conj base-chain (middlewares/file-info) index-dispatch) :route-name :index #_#_:constraints {:sexpr #"(?!ws$).*$"}]})
                                  (sort-by-position :route-name [:serve :index]))

       ::http/router            :linear-search ;; it’s fine we’ve got 1 or 2 routes
       ::http/host              host
       ::http/port              port
       ::http/request-logger    nil            ;; handled by i/trace
       ::http/container-options {:context-configurator
                                 (fn [^ServletContextHandler ctx]
                                   (doto ctx
                                     #_(.setGzipHandler (gzip-handler "GET" "POST"))
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

(comment
  ;; Default config example
  {:host   "localhost"
   :port   8080
   :scheme "http"})
