(ns wip.demo-custom-types
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [cognitect.transit :as t]))

;; Demo shows how to serialize custom types in Electric

(defrecord MyCustomType [field]) ; custom type

(def write-handler (t/write-handler
                     (fn [_] "wip.demo-custom-types/MyCustomType") ; this tag must be namespaced!
                     (fn [x] (into {} x))))

(def read-handler (t/read-handler map->MyCustomType))

; Todo cleanup, there are better ways to do this
#?(:clj (alter-var-root #'hyperfiddle.electric.impl.io/*write-handlers* assoc MyCustomType write-handler)) ; server: write only
#?(:cljs (set! hyperfiddle.electric.impl.io/*read-handlers* (assoc hyperfiddle.electric.impl.io/*read-handlers* "wip.demo-custom-types/MyCustomType" read-handler))) ; client: read only

(e/defn CustomTypes []
  (e/server
    (let [object (MyCustomType. "value")]
      (e/client
        (dom/dl
          (dom/dt (dom/text "type"))  (dom/dd (dom/text (pr-str (type object))))
          (dom/dt (dom/text "value")) (dom/dd (dom/text (pr-str object))))))))
