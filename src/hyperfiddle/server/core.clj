(ns hyperfiddle.server.core
  (:require ;; [hfdl.lang :as hfdl]
            ;; [hyperfiddle.server.entrypoint :as entrypoint]
            [hyperfiddle.server.websockets :as ws] ;; TODO restore
            [hyperfiddle.server.interceptors :as i]
            hyperfiddle.server.logging
            [hyperfiddle.server.routes :as routes]
            [io.pedestal.http :as http]
            [io.pedestal.http.ring-middlewares :as middlewares]
            [io.pedestal.http.route :refer [expand-routes]]
            [io.pedestal.http.secure-headers :as secure-headers]
            [io.pedestal.interceptor.helpers :refer [before]]
            [ring.middleware.file :as file]
            [taoensso.timbre :as log])
  (:import org.eclipse.jetty.server.handler.gzip.GzipHandler))

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

(defn build [{:keys [host port] :as config}]
  (let [base-chain (conj interceptors (i/with-config config))]
    (merge base-config
           {::http/routes
            (->> (expand-routes
                  #{["/assets/*file-path" :get [rewrite-resources-path (middlewares/file-info) (middlewares/file "resources/public")] :route-name :serve]
                    ["/*sexpr" :get (conj base-chain (middlewares/file-info) index-dispatch) :route-name :index]})
                 (routes/sort-by-position :route-name [:serve :index]))
            ::http/router            :linear-search ;; it’s fine we’ve got 1 or 2 routes
            ::http/host              host
            ::http/port              port
            ::http/request-logger    nil            ;; handled by i/trace
            ::http/container-options {:context-configurator
                                      (fn [^org.eclipse.jetty.servlet.ServletContextHandler ctx]
                                        (let [gzip-handler (GzipHandler.)]
                                          (.setGzipHandler ctx gzip-handler)
                                          (.addIncludedMethods gzip-handler (into-array ["GET" "POST"]))
                                          (ws/add-ws-endpoints config ctx ws/paths)))}})))

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
