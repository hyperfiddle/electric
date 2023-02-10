(ns ;^:dev/always ; recompile Photon entrypoint when Photon source changes
  user
  (:require hyperfiddle.electric
            hyperfiddle.rcf
            user-main))

(def photon-main (hyperfiddle.electric/boot (user-main/Main.)))
(defonce reactor nil)

(defn ^:dev/after-load ^:export start! []
  (set! reactor (photon-main
                 #(js/console.log "Reactor success:" %)
                 #(js/console.error "Reactor failure:" %)))
  (hyperfiddle.rcf/enable!))

(defn ^:dev/before-load stop! []
  (when reactor (reactor)) ; teardown
  (set! reactor nil))
