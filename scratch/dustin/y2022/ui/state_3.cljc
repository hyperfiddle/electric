(ns dustin.y2022.state-3
  (:require #?(:cljs goog.events)
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-dom :refer [node]]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.state-3)))

(p/defn Button [label busy]
  (dom/button {:aria-busy busy :disabled busy}
    label
    (dom/Event. "click" busy)))

(p/defn Demo []
  (p/client
    (p/with-cycle [busy false]
      (let [e (Button. "inc" busy)]
        (if (some? e)
          (try (p/server (Thread/sleep 500) false)
               (catch Pending _ true))
          busy)))))



; i think state machines are better written imperative?
; talk about the transitions, like a datomic tx; the value is a reduction of the transitions
; does this match the shape of callbacks?
#_        (when (some? toggle)
            (case status :closed :open :open :closed))