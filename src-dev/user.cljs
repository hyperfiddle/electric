(ns ^:dev/once user
  (:require
    goog.object
    ; Due to :require-macros, demos are loaded in JVM as well on shadow build
    user.demo-0-entrypoint
    user.demo-1-healthcheck
    user.demo-2-system-properties
    user.demo-3-webview
    user.demo-4-counter
    user.demo-5-button
    user.demo-6-bubbles
    user.demo-7-todos-basic
    user.seven-gui-1-counter
    user.seven-gui-2-temperature-converter
    user.seven-gui-4-timer
    user.seven-gui-5-crud
    wip.demo-two-clocks
    wip.orders-ui
    wip.demo-todos-stage
    wip.demo-logical-clock
    wip.example-router
    wip.hfql-links
    wip.explorer
    wip.demo-color
    wip.ui-components
    ;; requires running `yarn` for codemirror dependency
    ;; wip.demo-hfql
    ;; wip.editor
    )
  (:require-macros [user :refer [get-default-demo]]))

(defn runtime-resolve [exported-qualified-sym]
  (assert (qualified-symbol? exported-qualified-sym))
  (let [path-s        (str (munge (symbol (namespace exported-qualified-sym)))
                           "." (munge (name exported-qualified-sym)))
        path-segments (clojure.string/split path-s ".")]
    (goog.object/getValueByKeys js/window (clj->js path-segments))))

(defonce user-photon-main (get-default-demo))              ; lazy resolve
(defonce reactor nil)                                       ; save for debugging

(defn set-main [s]
  (set! user-photon-main (symbol s)))

(defn ^:dev/after-load ^:export start! [main]
  (when (or user-photon-main main)
    (set! reactor ((runtime-resolve (or main user-photon-main))       ; Photon main recompiles every reload, must re-resolve it
                   #(js/console.log "Reactor success:" %)
                   #(js/console.error "Reactor failure:" %)))))

(defn ^:dev/before-load stop! []
  (when reactor (reactor) #_"teardown")
  (set! reactor nil))

(defn browser-main! [photon-main-sym]
  ;(println ::received-reload-command photon-main-sym (type photon-main-sym))
  (set! user-photon-main photon-main-sym) (stop!) (start! nil))

(start! nil)
