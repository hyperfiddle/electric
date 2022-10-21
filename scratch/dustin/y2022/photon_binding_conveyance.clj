(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

(def ^:dynamic cc-db)
(defn orders [. needle]
  (d/q '[:in $ needle] cc-db needle))

(p/def rosie-db)

; enumerate the bindings you want captured
(let [orders' (binding [rosie/db (d/db .)]
                (partial-dynamic [cc-db db] orders))]
  (orders'))


(let [orders' (binding [rosie/db (d/db .)]
                ; captures all bindings, too much
                (bound-fn [needle] (orders needle)))]
  (orders'))

; tradeoff - user must list which dynamics to convey to clojure

; problems with automatic binding conveyance:
; - perf
; - you need to transfer everything all the time bc you
;   don't know which fn needs which dynamic

