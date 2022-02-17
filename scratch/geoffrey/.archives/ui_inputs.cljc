(ns geoffrey.ui-inputs
  (:require [hyperfiddle.api :as hf]))

(def set-input!
  #?(:cljs (fn [!needle]
             (fn [^js event]
               (reset! !needle (.. event -target -value))))))

(def *inputs (volatile! {}))

(defn get-input! [input]
  (get @*inputs (.-id input)))

(defn new-input! [initial-value onChange]
  (let [id    #?(:clj (java.util.UUID/randomUUID)
                 :cljs (random-uuid))
        input (hf/->Input id initial-value onChange)]
    #?(:cljs (vswap! *inputs assoc id input))
    input))
