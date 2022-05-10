(ns user.entrypoint
  (:require [triage.logger :as log]
            user.hytradboi
            user.orders-ui))

(def ^:export reactor)
(def main)                                                  ; assigned in index.html

(defn ^:dev/before-load stop! []
  (if reactor
    (do (log/info "Stopping reactor…")
        (reactor) ;; dispose
        (set! reactor nil)
        (log/info "Reactor stopped"))
    (log/info "Reactor already stopped")))

(defn ^:dev/after-load ^:export start! []
  (if-not reactor ; Browser state might be dirty (failed live code reload)
    (do (log/info "Starting reactor…")
        (set! reactor (main js/console.log #(do (log/error "Uncaugh error in main process" %)
                                                #_(stop!))))
        (log/info "Reactor started."))
    (log/info "Reactor already started")))

