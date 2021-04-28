(ns hyperfiddle.server.entrypoint
  (:require [hyperfiddle.server.routes :as routes]
            [hfdl.lang :refer [debug!]]))

(defn resolve* [f]
  (if (contains? routes/WHITELIST f)
    (fn [& args]
      (if-let [var (resolve f)]
        (apply var args)
        (throw (ex-info "This page is exposed but no implementation was found." {:page f}))))
    (throw (ex-info "404 Not found" {:not-found f}))))

(defn hf-run! [[f & args :as _route]]
  (try
    (debug! (apply (resolve* f) args))
    (catch Exception e
      ;; TODO Handle errors nicely here, forward them (sanitized) to client.
      (throw e))))

(comment
  (require '[dustin.fiddle-pages :refer [page-submissions]])
  (require '[hfdl.lang :refer [result]])

  (tests

   (def process (hf-run! "(dustin.fiddle-pages/page-submissions \"ali\")"))
   (result @process)
   ))
