(ns hyperfiddle.photon-jetty-server
  (:require [clojure.java.io :as io]
            [hyperfiddle.photon-jetty-adapter :as adapter]
            [hyperfiddle.logger :as log]
            [ring.adapter.jetty9 :as ring]
            [ring.middleware.basic-authentication :as auth]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.cookies :as cookies]
            [ring.middleware.file :refer [wrap-file]]
            [ring.util.response :as res])
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

(defn wrap-default-page [next-handler]
  (fn [ring-req]
    (case (:uri ring-req)
      ("" "/") (next-handler (assoc ring-req :uri "/index.html"))
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
(defn wrap-photon [next-handler]        ; Jetty 10 allows such handler
  (fn [ring-request]
    (if (ring/ws-upgrade-request? ring-request)
      (ring/ws-upgrade-response (adapter/photon-ws-adapter adapter/photon-ws-message-handler))
      (next-handler ring-request))))

(defn default-handler [ring-request]
  {:status 404
   :body "not found"})

(defn file-exsist? [path] (.exists (io/as-file path)))

(defn wrap-spa [next-handler]
  (fn [ring-req]
    (next-handler (assoc ring-req :uri "/index.html" ))))

(defn- add-gzip-handler [server]
  (.setHandler server
    (doto (GzipHandler.)
      #_(.setIncludedMimeTypes (into-array ["text/css" "text/plain" "text/javascript" "application/javascript" "application/json" "image/svg+xml"]))
      (.setMinGzipSize 1024)
      (.setHandler (.getHandler server)))))

(defn start-server! [{:keys [resources-path port allow-symlinks?] :as config
                      :or   {resources-path  "resources/public"
                             allow-symlinks? false}}]
  (try
    (let [jetty-handler (cond-> #'default-handler ; these compose as functions, so are applied bottom up
                                (file-exsist? resources-path) (wrap-file resources-path) ; 6. serve it
                                true (wrap-content-type) ; 5. detect content
                                true (wrap-spa) ; 4. otherwise fallback to default page file
                                (file-exsist? resources-path) (wrap-file resources-path {:allow-symlinks? allow-symlinks?}) ; 3. serve static file if it exists
                                true (wrap-content-type) ; 2. detect content (e.g. for index.html)
                                true (wrap-default-page) ; 1. route
                                true (wrap-no-cache) ; TODO disable in prod
                                #_(wrap-photon))
          jetty-options (merge {:port 8080
                                :join? false
                                ;; Jetty 9 forces us to declare WS paths out of a ring handler.
                                :websockets {"/" (fn [ring-req]
                                                   (adapter/photon-ws-adapter
                                                     (partial adapter/photon-ws-message-handler
                                                              (-> ring-req
                                                                  (auth/basic-authentication-request authenticate)
                                                                  (cookies/cookies-request)))))}
                                :configurator add-gzip-handler}
                               config)
          server (ring/run-jetty jetty-handler jetty-options)
          final-port (-> server (.getConnectors) first (.getPort))]
      (println "\n👉 App server available at" (str "http://" (:host config) ":" final-port "\n")))

    (catch IOException err
      (if (instance? BindException (ex-cause err))
        (do (log/warn "Port" port "was not available, retrying with" (inc port))
            (start-server! (update config :port inc)))
        (throw err)))))

