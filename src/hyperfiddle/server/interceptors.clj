(ns hyperfiddle.server.interceptors
  (:require ;; [cognitect.transit :as transit]
            ;; contrib.pprint ;; TODO restore
            ;; [hypercrud.transit :as hc-t]
            ;; [io.pedestal.http :as http]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            ;; [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.content-negotiation :as content-negotiation]
            ;; [io.pedestal.http.ring-middlewares :as ring-middleware]
            [io.pedestal.interceptor.helpers :as interceptor]
            [hyperfiddle.dev.logger :as log])
  (:import [java.io #_OutputStream OutputStreamWriter]
           java.util.UUID
           org.apache.commons.lang3.StringEscapeUtils))

;; For POST
;; (def body-params (-> (body-params/body-params
;;                       (body-params/default-parser-map
;;                        :edn-options     {:readers *data-readers*}
;;                        #_#_:transit-options [{:handlers @hc-t/read-handlers}]))
;;                      (update :enter (fn [enterf]
;;                                       (fn [context]
;;                                         (let [{:keys [edn-params transit-params json-params body-params]} (:request (enterf context))]
;;                                           (update context :request assoc :body-params (or json-params edn-params transit-params body-params))))))))

;; (def cookies ring-middleware/cookies)

(def trace (interceptor/around ::trace
                               (fn trace-request [{:keys [request] :as context}]
                                 (let [id (UUID/randomUUID)
                                       {:keys [remote-addr server-name server-port request-method uri protocol]}
                                       ,, request]
                                   (log/trace {:id          id
                                              :remote-addr remote-addr
                                              :host        server-name
                                              :port        server-port
                                              :method      (str/upper-case (name request-method))
                                              :uri         uri
                                              :protocol    protocol
                                              :referer     (get-in request [:headers "referer"])
                                              :user-agent  (get-in request [:headers "user-agent"])})
                                   (update context :request assoc :id id)))
                               (fn trace-response [{:keys [request response] :as context}]
                                 (log/debug "request" (str (:id request)) "=> HTTP" (:status response))
                                 context)))

(defn- print-fn
  [prn-fn]
  (fn [output-stream]
    (with-open [writer (OutputStreamWriter. output-stream)]
      (binding [*out* writer]
        (prn-fn))
      (.flush writer))))

(def ^:private content-transformers
  {"text/plain"
   (fn [body]
     (print-fn #(pr body)))

   "text/html"
   (fn [body]
     #_(binding [clojure.pprint/*print-right-margin* 140]
         (let [body-str (->> (with-out-str (contrib.pprint/pprint body))
                             (StringEscapeUtils/escapeHtml4)
                             (format "<html><body><pre>%s</pre></body></html>"))]
           (print-fn #(.write *out* body-str))))
     (let [body-str (->> (with-out-str (pprint body))
                         (StringEscapeUtils/escapeHtml4)
                         (format "<html><body><pre>%s</pre></body></html>"))]
       (print-fn #(.write *out* body-str))))

   ;; EDN JSON, Transit only through WS.
   ;; "application/json"
   ;; (fn [body]
   ;;   (print-fn #(http/json-print body)))

   ;; "application/edn"
   ;; (fn [body]
   ;;   ;; https://github.com/hyperfiddle/hyperfiddle.net/issues/38
   ;;   (print-fn #(pr body)))

   ;; "application/transit+json"
   ;; (fn [body]
   ;;   (fn [^OutputStream output-stream]
   ;;     (transit/write (transit/writer output-stream :json {:handlers @hc-t/write-handlers}) body)
   ;;     (.flush output-stream)))

   ;; "application/transit+msgpack"
   ;; (fn [body]
   ;;   (fn [^OutputStream output-stream]
   ;;     (transit/write (transit/writer output-stream :msgpack {:handlers @hc-t/write-handlers}) body)
   ;;     (.flush output-stream)))

   })

(def negociate (content-negotiation/negotiate-content (keys content-transformers)))

(defn- coerce-to [response content-type]
  (-> response
      (update :body (get content-transformers content-type))
      (assoc-in [:headers "Content-Type"] content-type)))

(def auto-content-type
  (interceptor/after ::auto-content-type
                     (fn [{:keys [request response] :as context}]
                       (if (nil? (get-in response [:headers "Content-Type"]))
                         (update context :response coerce-to (get-in request [:accept :field] "text/plain"))
                         context))))

(defn with-config [config]
  (interceptor/before ::with-config (fn [context] (assoc context :config config))))
