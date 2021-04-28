(ns hyperfiddle.server.entrypoint
  (:require [hyperfiddle.common.links :as links]
            [hfdl.lang :refer [debug!]]))

;; TODO should either come from config or reflected from metas.
(def whitelist `#{dustin.fiddle-pages/page-submissions})

;; TODO: use a real parser like edamame to be able to diagnose what's wrong with
;; a link. This is primitive routing, just parse sexp.
(defn route [link]
  (links/link->sexp link))

(defn resolve* [f]
  (if (contains? whitelist f)
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
