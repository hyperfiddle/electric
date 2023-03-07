(ns hyperfiddle.electric-jetty-server
  (:require [clojure.java.io :as io]
            [hyperfiddle.electric-jetty-adapter :as adapter]
            [hyperfiddle.logger :as log]
            [ring.adapter.jetty9 :as ring]
            [ring.middleware.basic-authentication :as auth]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.cookies :as cookies]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as res]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import [java.io IOException]
           [java.net BindException]
           [org.eclipse.jetty.server.handler.gzip GzipHandler]))

(defn authenticate [username password] username) ; demo (accept-all) authentication

(defn wrap-demo-authentication [next-handler]
  ;; Authentication demo, accept any user/password, stores the username in a cookie
  (-> (fn [ring-req]
        (let [res (next-handler ring-req)]
          (if-let [username (:basic-authentication ring-req)]
            (res/set-cookie res "username" username {:http-only true})
            res)))
    (cookies/wrap-cookies)
    (auth/wrap-basic-authentication authenticate)))

(defn wrap-router [next-handler]
  (fn [ring-req]
    (case (:uri ring-req)
      ("/auth") (let [response  ((wrap-demo-authentication next-handler) ring-req)]
                  (if (= 401 (:status response)) ; authenticated?
                    response                     ; send response to trigger auth prompt
                    (-> (res/status response 302) ; redirect
                        (res/header "Location" "/"))))
      ;; delegate to next middleware
      (next-handler ring-req))))

(defn wrap-no-cache [next-handler]
  (fn [ring-req]
    (assoc-in (next-handler ring-req) [:headers "Cache-Control"] "No-Store")))

#_
(defn wrap-electric-websocket [next-handler]        ; Jetty 10 allows such handler
  (fn [ring-request]
    (if (ring/ws-upgrade-request? ring-request)
      (ring/ws-upgrade-response (adapter/electric-ws-adapter adapter/electric-ws-message-handler))
      (next-handler ring-request))))

(defn default-handler [ring-request]
  {:status 404
   :body "not found"})

(defn file-exsist? [path] (.exists (io/as-file path)))

(defn template [string opts]
  (reduce-kv (fn [r k v] (str/replace r (str "$" k "$") v)) string opts))

(defn get-modules [js-path]             ; TODO improve error message if `manifest` is missing
  (->> (slurp (io/resource (str js-path "/manifest.edn")))
    (edn/read-string)
    (reduce (fn [r module] (assoc r (keyword "hyperfiddle.client.module" (name (:name module))) (str "/js/" (:output-name module)))) {})))

(defn wrap-index-page [next-handler resources-path js-path]
  (fn [ring-req]
    (if-let [response (res/resource-response (str resources-path "/index.html"))]
      (-> (res/response (template (slurp (:body response)) (get-modules js-path))) ; TODO should be cached in prod mode
        (res/header "Last-Modified" (get-in response [:headers "Last-Modified"]))
        (res/content-type "text/html"))
      (next-handler ring-req))))

(defn- add-gzip-handler [server]
  (.setHandler server
    (doto (GzipHandler.)
      #_(.setIncludedMimeTypes (into-array ["text/css" "text/plain" "text/javascript" "application/javascript" "application/json" "image/svg+xml"]))
      (.setMinGzipSize 1024)
      (.setHandler (.getHandler server)))))

(defn get-server-version [js-path]
  (if-let [version-file (io/resource (str js-path "/.version"))]
    (slurp version-file)
    ""))

(defn wrap-reject-stale-client [next-handler js-path]
  (let [server-version (get-server-version js-path)]
    (fn [ring-req]
      (let [client-version (get-in ring-req [:query-params "version"])]
        (cond
          (= "dev" client-version)          (next-handler ring-req)
          (= client-version server-version) (next-handler ring-req)
          :else (adapter/reject-websocket-handler 1008 "stale client") ; https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1
          )))))

(defn start-server! [{:keys [resources-path js-path port allow-symlinks?] :as config
                      :or   {resources-path  "public"
                             js-path         "public/js"
                             allow-symlinks? false}}]
  (try
    (let [ring-handler (-> #'default-handler ; these compose as functions, so are applied bottom up
                         (wrap-index-page resources-path js-path) ; 4. otherwise fallback to default page file
                         (wrap-resource resources-path {:allow-symlinks? allow-symlinks?}) ; 3. serve static file from jar
                         (wrap-content-type) ; 2. detect content (e.g. for index.html)
                         (wrap-router) ; 1. route
                         #_(wrap-electric-websocket)) ; Jetty 10 ws configuration with userland entrypoint
          ring-websocket-handler (fn [next-handler]
                                   (-> (cookies/wrap-cookies next-handler)
                                     (wrap-reject-stale-client js-path)
                                     (wrap-params)))

          ; For Jetty 10 (NOT Java 8 compatible), use `wrap-electric-websocket` as above
          ; For Jetty 9 (Java 8 compatible), use :websocket jetty-option as below
          ; This hardcoded configuration is for Jetty 9 (for Java 8 compat).
          ; Java 8 is macos system java, and supporting it simplifies setup for non-Clojure devs.

          jetty-options (merge {:port 8080
                                :join? false
                                ;; For Jetty 9 forces us to declare WS paths out of a ring handler.
                                :websockets {"/" (ring-websocket-handler
                                                   (fn [ring-req]
                                                     (adapter/electric-ws-adapter
                                                       (partial adapter/electric-ws-message-handler
                                                         (auth/basic-authentication-request ring-req authenticate)))))}
                                :configurator add-gzip-handler}
                               config)
          server (ring/run-jetty ring-handler jetty-options)
          final-port (-> server (.getConnectors) first (.getPort))]
      (println "\n👉 App server available at" (str "http://" (:host config) ":" final-port "\n"))
      server)

    (catch IOException err
      (if (instance? BindException (ex-cause err))
        (do (log/warn "Port" port "was not available, retrying with" (inc port))
            (start-server! (update config :port inc)))
        (throw err)))))

