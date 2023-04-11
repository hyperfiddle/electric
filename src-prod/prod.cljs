(ns prod
  (:require hyperfiddle.electric
            user-main))

(def electric-main (hyperfiddle.electric/boot (user-main/Main.)))

(defn start! []
  (electric-main
    #(js/console.log "Reactor success:" %)
    #(js/console.error "Reactor failure:" %)))
