(ns hyperfiddle.server.entrypoint
  (:require [hyperfiddle.server.routes :as routes]
            [hfdl.lang :refer [dataflow]]
            [hfdl.impl.trace :refer [system debug*]]))

(defn resolve* [f]
  (if (contains? routes/WHITELIST f)
    (if-let [var (resolve f)]
      (fn [& args]
        (apply var args))
      (throw (ex-info "This page is exposed but no implementation was found." {:page f})))
    (throw (ex-info "404 Not found" {:not-found f}))))

(defn hf-run! [[f & args :as _route]]
  (try
    (apply (resolve* f) args)
    (catch Exception e
      ;; TODO Handle errors nicely here, forward them (sanitized) to client.
      (dataflow {:error {:message (ex-message e)
                         :data    (ex-data e)}}))))

(defn eval-fiddle! [>route]
  (let [!result (atom nil)]
    ((system (debug* !result (dataflow @(hf-run! @>route)))) prn prn)
    @!result))

(comment
  (require '[dustin.fiddle-pages :refer [page-submissions]])
  (require '[hfdl.lang :refer [result]])

  (tests

   (def process (hf-run! "(dustin.fiddle-pages/page-submissions \"ali\")"))
   (result @process)
   ))
