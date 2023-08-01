(ns hyperfiddle.electric-fulcro-dom-adapter
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2]
            #?(:cljs [goog.object :as gobj])
            #?(:cljs [com.fulcrologic.fulcro.components :as comp :refer [defsc]])
            #?(:cljs [com.fulcrologic.fulcro.dom :as dom :refer [div]]))
  #?(:cljs (:require-macros [hyperfiddle.electric-fulcro-dom-adapter])))

#?(:cljs
   (defsc ElectricBridge [this props]
     {:componentDidMount    (fn [^js this]
                              (let [ref     (gobj/get this "container")
                                    program ((::electric-program (comp/props this)) ref)
                                    reactor (program
                                              #(js/console.log "Reactor success:" %)
                                              #(js/console.error "Reactor failure:" %)) ]
                                (comp/set-state! this {:reactor reactor})))
      :componentWillUnmount (fn [this] ((:reactor (comp/get-state this))))}
     (div (-> props
            (dissoc ::electric-program)
            (assoc :ref (fn [r] (gobj/set this "container" r)))))))

#?(:cljs
   (def ui-electric-bridge (comp/factory ElectricBridge)))

(defmacro with-electric
  "Runs `body` as an Electric program. Mounts in a container div, customizable with `props`.
  Lexical scope is conveyed.

  e.g.
  ```
  (require '[hyperfiddle.electric-fulcro-dom-adapter :as fa])
  (require '[com.fulcrologic.fulcro.dom :as fdom])
  (require '[hyperfiddle.electric-dom2 :as edom])
  (require '[com.fulcrologic.fulcro.routing.dynamic-routing :as dr])

  (defsc Page1 []
    {:route-segment [\"page-1\"]]}
    (fdom/div \"page-1\"))

  (defsc Page2 [this]
    (fa/with-electric {:id \"my-electric-app\"}
      (edom/button
        (edom/on! \"click\" (fn [_e] (dr/change-route this (dr/path-to Page1))))
        (edom/text \"Go to Page1\"))))
  ```"
  [props & body]
  `(ui-electric-bridge (merge ~props {::electric-program (fn [ref#]
                                                           (e/boot
                                                             (binding [hyperfiddle.electric-dom2/node ref#]
                                                               ~@body)))})))


