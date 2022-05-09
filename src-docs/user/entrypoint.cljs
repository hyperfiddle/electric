(ns user.entrypoint
  (:require [hyperfiddle.client :refer [client]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.dev.logger :as log]
            user.hytradboi
            user.orders-ui))

(def main
  (client
   (p/main
    (binding [dom/parent (dom/by-id "root")]
      (dom/div
       (dom/attribute "id" "main")
       (dom/class "browser")
       (dom/div
        (dom/class "view")
        (new user.hytradboi/view)
        #_(new user.orders-ui/Orders)))))))

(def ^:export reactor)

(defn ^:dev/before-load stop! []
  (if reactor
    (do (log/info "Stopping reactor…")
        (reactor) ;; dispose
        (set! reactor nil)
        (log/info "Reactor stopped"))
    (log/info "Reactor already stopped")))

(defn ^:dev/after-load ^:export start! []
  (if-not reactor
    (do (log/info "Starting reactor…")
        (set! reactor (main js/console.log #(do (log/error "Uncaugh error in main process" %)
                                                #_(stop!))))
        (log/info "Reactor started."))
    (log/info "Reactor already started")))

