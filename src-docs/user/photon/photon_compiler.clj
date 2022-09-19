(ns user.photon.photon-compiler
  (:require [clojure.datafy :refer [datafy]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

(tests "Photon baseline program - a dataflow diamond"
  (def !x (atom 0))
  (def dispose (p/run
                 (let [x (p/watch !x)                       ; reactive x
                       a (inc x)]                           ; reactive a depends on reactive x
                   (tap (+ a (inc x))))))                     ; reactive +
  % := 2
  (swap! !x inc)
  % := 4
  (swap! !x inc)
  % := 6
  (dispose))

; Since in Photon everything is a missionary flow, it feels like nothing is, so you don't see the flow types.
; (except at the missionary/photon interop boundary)

(tests
  "Approximately equivalent program as missionary, for understanding"
  (def !x (atom 0))
  (defn user []
    (let [<x (m/signal! (m/watch !x))
          <a (m/signal! (m/latest inc <x))]
      (m/latest rcf/tap (m/latest + <a (m/latest inc <x)))))
  (def main (m/reactor (m/stream! (user))))                 ; entrypoint that runs for effect
  (def dispose (main (fn [_] (rcf/tap ::success))             ; start process, will run until disposed
                     (fn [err] (rcf/tap ::error) (rcf/tap err))))
  % := 2
  (swap! !x inc)
  % := 4
  (swap! !x inc)
  % := 6
  (dispose)
  [% (datafy %)] := [::error {:cause "Watch cancelled." :via _ :trace _}]

  ; Why does reactor terminate with an error?
  ; Generally speaking, to stop listening to a signal is a failure, because you have not reached the end of the signal.
  ; This watch signal is infinite, so the only way to terminate is to stop listening before the end.
  ; The watch throws on cancellation because this is how the supervision tree cleanup lifecycle works. We need to
  ; dispose any subscriptions and prevent any pending transfers.
  )
