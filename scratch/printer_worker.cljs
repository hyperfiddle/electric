(ns hyperfiddle.client.printer-worker
  (:require [clojure.string :as str]
            [hyperfiddle.common.transit :as transit]
            [fipp.edn :as fipp]
            [fipp.clojure :as fipp-clj]))

(defn pprint [o & [columns]]
  (str/trimr
   ;; (binding [pprint/*print-right-margin* (or columns pprint/*print-right-margin*)])
   (with-out-str (fipp-clj/pprint o))))

(def encoder (js/TextEncoder.))
(def decoder (js/TextDecoder.))

(defn handle [^js e]
  (let [start   (js/self.performance.now)
        in      (->> (.. e -data -buffer)
                   (.decode decoder)
                   (transit/decode))
        columns (.. e -data -columns)
        ;; TODO this create a new buffer even if we already have one at hand, we
        ;; should reuse allocated memory (reuse buffer?) and just send back a
        ;; pointer. Or we could use shared memory space.
        ;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer
        out     (.encode encoder (pprint in columns))]
    #_(js/console.log #js{:in  in
                          :out (pprint in)})
    (js/postMessage #js{:id     (.. e -data -id)
                        :buffer (.-buffer out)
                        :start  start
                        :end    (js/self.performance.now)}
                    #js[(.-buffer out)])))

(defn init [] (js/self.addEventListener "message" handle))
