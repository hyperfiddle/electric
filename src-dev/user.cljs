(ns ^:dev/always user ; recompile Photon entrypoint when Photon source changes
  (:require clojure.string
            goog.object
            hyperfiddle.photon
            hyperfiddle.photon-dom-test
            hyperfiddle.rcf
            user-main)
  (:require-macros [user :refer [get-main]]))

(defn runtime-resolve [exported-qualified-sym]
  (assert (qualified-symbol? exported-qualified-sym))
  (let [path-s        (str (munge (symbol (namespace exported-qualified-sym)))
                           "." (munge (name exported-qualified-sym)))
        path-segments (clojure.string/split path-s ".")]
    (goog.object/getValueByKeys js/window (clj->js path-segments))))

(def ^:export photon-main (hyperfiddle.photon/boot (user-main/Main.)))
(defonce user-photon-main (get-main user/photon-main)) ; lazy resolve, dev user can alter at REPL
(defn set-main [s] (set! user-photon-main (symbol s)))
(defonce reactor nil)

(defn ^:dev/after-load ^:export start! [main]
  (when (or user-photon-main main)
    (set! reactor ((runtime-resolve (or main user-photon-main))       ; Photon main recompiles every reload, must re-resolve it
                   #(js/console.log "Reactor success:" %)
                   #(js/console.error "Reactor failure:" %))))
  (hyperfiddle.rcf/enable!))

(defn ^:dev/before-load stop! []
  (when reactor (reactor) #_"teardown")
  (set! reactor nil))

(defn browser-main! "hot switch reactor entrypoint from CLJS REPL" [photon-main-sym]
  ;(println ::received-reload-command photon-main-sym (type photon-main-sym))
  (set! user-photon-main photon-main-sym) (stop!) (start! nil))
