(ns ^:dev/once user
  (:require
    clojure.string
    goog.object
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom :as dom]
    hyperfiddle.photon-dom-test
    hyperfiddle.rcf
    user.demo-entrypoint

    ; Due to :require-macros, demos are loaded in JVM as well on shadow build
    user.demo-1-hello-world
    user.demo-2-toggle
    user.demo-3-system-properties
    user.demo-4-chat
    user.demo-4-webview
    user.demo-5-todos-basic
    user.demo-5-todomvc
    user.demo-6-two-clocks
    user.demo-7-explorer
    user.demo-8-10k-elements
    user.auth
    user.seven-gui-1-counter
    user.seven-gui-2-temperature-converter
    user.seven-gui-4-timer
    user.seven-gui-5-crud
    geoffrey.popover
    wip.demo-bubbles
    ;wip.orders-ui
    wip.demo-bubbles
    wip.demo-color
    wip.demo-hfql
    #_wip.editor           ; npm install
    wip.demo-todos-stage
    wip.demo-logical-clock
    wip.example-router
    wip.hfql-links
    wip.ui-components
    wip.popover)
  (:import [hyperfiddle.photon Pending])
  (:require-macros [user :refer [get-main]]))

(defn runtime-resolve [exported-qualified-sym]
  (assert (qualified-symbol? exported-qualified-sym))
  (let [path-s        (str (munge (symbol (namespace exported-qualified-sym)))
                           "." (munge (name exported-qualified-sym)))
        path-segments (clojure.string/split path-s ".")]
    (goog.object/getValueByKeys js/window (clj->js path-segments))))

(def ^:export demo-main
  (p/boot
    (try
      (binding [dom/node (dom/by-id "root")]
        (user.demo-entrypoint/App.))
      (catch Pending _))))

(defonce user-photon-main (get-main user/demo-main)) ; lazy resolve
(defn set-main [s] (set! user-photon-main (symbol s)))
(defonce reactor nil)

;^:dev/after-load -- temporarily disable hot code reloading
(defn ^:export start! [main]
  (when (or user-photon-main main)
    (set! reactor ((runtime-resolve (or main user-photon-main))       ; Photon main recompiles every reload, must re-resolve it
                   #(js/console.log "Reactor success:" %)
                   #(js/console.error "Reactor failure:" %)))))

;^:dev/before-load -- temporarily disable hot code reloading
(defn stop! []
  (when reactor (reactor) #_"teardown")
  (.. js/document (getElementById "root") (replaceChildren)) ; temporary workaround for https://github.com/hyperfiddle/photon/issues/10
  (set! reactor nil))

(defn browser-main! "hot switch reactor entrypoint from CLJS REPL" [photon-main-sym]
  ;(println ::received-reload-command photon-main-sym (type photon-main-sym))
  (set! user-photon-main photon-main-sym) (stop!) (start! nil))

(start! nil)
