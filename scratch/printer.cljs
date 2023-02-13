(ns hyperfiddle.client.printer
  (:require [hyperfiddle.common.transit :as transit]))

(defonce worker (atom nil))

;; Hold {callback-id callback}
(defonce callbacks (atom {}))

;; To encode from/to Uint8Array,
;; https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder
(def encoder (and (exists? js/TextEncoder)
                  (js/TextEncoder.)))
(def decoder (and (exists? js/TextDecoder)
                  (js/TextDecoder.)))

(defn- ms "round number to 2 decimals, for milisec precision" [x]
  (/ (.round js/Math (* 100 x)) 100))

(defn now
  "Current time in sub-millisecond resolution and such that it is not subject to
  system clock skew or adjustments. Good enough to generate a unique id in this
  context."
  []
  (js/window.performance.now))

(defn call-cc [^js e]
  (let [result       (.decode decoder (.. e -data -buffer))
        receive-time (now)
        id           (.. e -data -id)
        start        (.. e -data -start)
        end          (.. e -data -end)
        thread-time  (- end start)
        total-time   (- receive-time id)
        overhead     (- total-time thread-time)]
    (if-let [f (get @callbacks id)]
      (do
        #_(js/console.debug "pprint" #js{:id        id
                                         :total-ms  (ms total-time)
                                         :thread-ms (ms thread-time)
                                         :coord-ms  (ms overhead)})
        (swap! callbacks dissoc id)
        (f result))
      (throw (ex-info "Worker continuation not found" {:id    id
                                                       :value result})))))

(defn get-worker! []
  (or @worker
      (reset! worker (doto (js/Worker. "/assets/js/printer-worker.js")
                       (.addEventListener "message" (fn [e]
                                                      ;; Var indirection
                                                      (call-cc e)))))))

(defn send! [value columns callback]
  (let [id     (now)
        buffer (.encode encoder (transit/encode value))]
    (swap! callbacks assoc id callback)
    (.postMessage (get-worker!)
                  #js{:id      id
                      :columns columns
                      :buffer  (.-buffer buffer)}
                  #js[(.-buffer buffer)])))

(defn pprint
  "Set columns to nil for default behavior"
  [o columns callback]
  (send! o columns callback))
