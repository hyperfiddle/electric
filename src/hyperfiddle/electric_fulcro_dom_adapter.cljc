(ns hyperfiddle.electric-fulcro-dom-adapter
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2]
            #?(:cljs [goog.object :as gobj])
            #?(:cljs [com.fulcrologic.fulcro.components :as comp :refer [defsc]])
            #?(:cljs [com.fulcrologic.fulcro.dom :as dom :refer [div]]))
  #?(:cljs (:require-macros [hyperfiddle.electric-fulcro-dom-adapter])))

(def ^:private !props "Contains props of all fulcro->electric instances, keyed by `id`"
  ;; Only needed under Electric IC first iteration. Next electric version will
  ;; allow server and client to inject arguments.
  (atom {}))

(e/def props (e/watch !props)) ; A single top level watch for better perfs
(e/defn GetProps [id] (get props id))

#?(:cljs
   (defsc ElectricBridge [this props]
     {:componentDidMount    (fn [^js this]
                              (let [reactor ((::electric-program (comp/props this)))
                                    process (reactor
                                              #(js/console.log "Reactor success:" %)
                                              #(js/console.error "Reactor failure:" %))]
                                (swap! !props assoc-in [(::id (comp/props this)) ::react-ref] (gobj/get this "container"))
                                (comp/set-state! this {:process process})))
      :componentWillUnmount (fn [this]
                              ((:process (comp/get-state this)))
                              (swap! !props dissoc (::id (comp/props this))))}
     (div (-> props
            (dissoc ::electric-program)
            (assoc :ref (fn [r] (gobj/set this "container" r)))))))

#?(:cljs (def ui-electric-bridge (comp/factory ElectricBridge)))

(defmacro run-electric!
  "Runs an electric program inside a fulcro component. Only one instance of an
  electric program can run on a given fulcro page. i.e. one can have multiple
  `run-electric!` calls on a fulcro page, but they must all run different
  electric programs.

  - `props`: a props map passed to a fulcro-dom wrapper div. Electric will mount
            inside of this div.
  - `Electric-Entrypoint-Fn`: fully-qualified symbol referring to an Electric e/def or e/defn to run.
  - `args-map`: an arbitrary clojure map passed to `Electric-Entrypoint-Fn` as
                first argument. Use it to convey reactive values from fulcro to electric.

Example:
```
(ns fulcro-electric-example
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-fulcro-dom-adapter :as fulcro-adapter]
            [com.fulcrologic.fulcro.dom :as fulcro-dom])

(e/defn ElectricApp [ring-request] ; starts on the server
  (e/client
    (let [{:keys [this OtherPage ::fulcro-adapter/react-ref]} (fulcro-adapter/GetProps. `ElectricApp)]
      (binding [dom/node react-ref]
        (dom/button
          (dom/on! \"click\" (fn [_e] (dr/change-route this (dr/path-to OtherPage))))
          (dom/text \"Go to OtherPage\"))))))

(defsc OtherPage []
  {:route-segment [\"page-1\"]]}
  (fulcro-dom/div \"page-1\")

(defsc MainPage [this]
  (run-electric! {:id \"my-electric-app-in-main-page\"} fulcro-electric-example/ElectricApp {:this this, :OtherPage OtherPage}))
```
  "
  [props Electric-Entrypoint-Fn args-map]
  (assert (qualified-symbol? Electric-Entrypoint-Fn) "Electric-Entrypoint-Fn must be a fully qualified symbol resolving to an e/def or e/defn.")
  `(do
     (swap! !props update '~Electric-Entrypoint-Fn merge ~args-map)
     (ui-electric-bridge (merge ~props {::id '~Electric-Entrypoint-Fn ::electric-program (fn [] (e/boot-client {} ~Electric-Entrypoint-Fn nil))}))))

