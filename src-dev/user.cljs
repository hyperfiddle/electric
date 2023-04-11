(ns ^:dev/always ; recompile Electric entrypoint when Electric source changes
  user
  (:require hyperfiddle.electric
            hyperfiddle.rcf
            user-main))

(def electric-main (hyperfiddle.electric/boot (user-main/Main.)))
(defonce reactor nil)

(defn ^:dev/after-load ^:export start! []
  (set! reactor (electric-main
                 #(js/console.log "Reactor success:" %)
                 #(js/console.error "Reactor failure:" %)))
  (hyperfiddle.rcf/enable!))

(defn ^:dev/before-load stop! []
  (when reactor (reactor)) ; teardown
  (set! reactor nil))
