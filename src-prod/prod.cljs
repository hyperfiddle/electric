(ns prod
  (:require hyperfiddle.electric
            user-main))

(def electric-main (hyperfiddle.electric/boot (user-main/Main.)))

(defn start! []
  (electric-main
    #(js/console.log "Reactor success:" %)
    (fn [error]
      (case (:hyperfiddle.electric/type (ex-data error))
        :hyperfiddle.electric-client/stale-client (do (js/console.log "Server and client version mismatch. Refreshing page.")
                                                      (.reload (.-location js/window)))
        (js/console.error "Reactor failure:" error)))))
