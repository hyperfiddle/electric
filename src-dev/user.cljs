(ns user
  (:require
    goog.object
    ; Due to :require-macros, demos are loaded in JVM as well on shadow build
    user.demo-healthcheck
    user.demo-system-properties
    user.demo-webview
    user.demo-button
    user.demo-counter
    user.app-starter
    wip.demo-two-clocks
    wip.orders-ui
    wip.demo-hfql
    wip.demo-todos-basic
    wip.demo-todos-stage
    wip.demo-logical-clock
    ))

(defn runtime-resolve [exported-qualified-sym]
  (let [path-s        (str (munge (symbol (namespace exported-qualified-sym)))
                           "." (munge (name exported-qualified-sym)))
        path-segments (clojure.string/split path-s ".")]
    (goog.object/getValueByKeys js/window (clj->js path-segments))))

(defonce user-photon-main `user.demo-healthcheck/main)      ; lazy resolve
(defonce reactor nil)                                       ; save for debugging

(defn ^:dev/after-load ^:export start! []
  (when user-photon-main
    (set! reactor ((runtime-resolve user-photon-main)       ; Photon main recompiles every reload, must re-resolve it
                   #(js/console.log "Reactor success:" %)
                   #(js/console.error "Reactor failure:" %)))))

(defn ^:dev/before-load stop! []
  (when reactor (reactor) #_"teardown")
  (set! reactor nil))

(defn browser-main! [photon-main-sym]
  ;(println ::received-reload-command photon-main-sym (type photon-main-sym))
  (set! user-photon-main photon-main-sym) (stop!) (start!))
