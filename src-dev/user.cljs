(ns ^:dev/once user
  (:require
    goog.object
    hyperfiddle.photon-dom-test
    hyperfiddle.rcf
    ; Due to :require-macros, demos are loaded in JVM as well on shadow build
    user.demo-entrypoint
    user.demo-1-counter
    user.demo-2-chat
    user.demo-3-system-properties
    user.demo-4-webview
    user.demo-5-todos-basic
    user.demo-6-two-clocks
    user.demo-7-explorer
    user.demo-8-10k-elements
    user.seven-gui-1-counter
    user.seven-gui-2-temperature-converter
    user.seven-gui-4-timer
    user.seven-gui-5-crud
    user.todomvc
    geoffrey.popover
    wip.demo-bubbles
    ;wip.orders-ui
    wip.demo-bubbles
    wip.demo-color
    #_wip.demo-hfql        ; npm install
    #_wip.editor           ; npm install
    wip.demo-todos-stage
    wip.demo-logical-clock
    wip.example-router
    wip.hfql-links
    wip.ui-components)
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
