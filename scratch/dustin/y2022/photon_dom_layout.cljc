(ns dustin.y2022.photon-dom-layout
  (:require [missionary.core :as m]
            [clojure.core.async :as a]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  (:import [hyperfiddle.photon Pending Failure]
           (missionary Cancelled)))

(hyperfiddle.rcf/enable!)

(defn inc-rf [acc _] (inc acc))

(p/defn Click-counter [label]
  (dom/div
    (dom/span (dom/text label))
    (dom/input {:type "button"})

    ::n (if (p/impulse p/frame (dom/>events "input")) 1 0)
    ::n (if (p/impulse p/frame (dom/>events "input")) 1 0)
    ::n (if (p/impulse p/frame (dom/>events "input")) 1 0)
    ))

(p/defn Counting-component []
  (let [{:keys [::<n] :as el} (Click-counter. (p/watch !label))]
    (dom/div
      (dom/text "count: ")
      (dom/text (new (m/reductions inc-rf 0 <n)))
      (Click-counter. (p/watch !label))
      (dissoc el ::<n))))



(defmacro ui-thunk [& body]
  `(let [val# (atom nil)
         elem# (p/fn [] (reset! val# (do ~@body)))]
     [(p/watch val) elem#]))

(let [[v e] (ui-thunk (ui/input {}))]
  (dom/p (dom/text "val is " v))
  (e.))
