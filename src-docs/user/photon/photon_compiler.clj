(ns user.photon.photon-compiler
  (:require [clojure.datafy :refer [datafy]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-impl.compiler :refer [analyze]]
            [hyperfiddle.photon-impl.runtime :as r :refer [emit]]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

; Is there a missionary operation one can equivalently rewrite as a p/fn?
; Like instead of doing an m/eduction one brings the data into the photon side of world and does the same there?

(tests "Photon baseline program - a dataflow diamond"
  (def !x (atom 0))
  (def dispose (p/run
                 (let [x (p/watch !x)
                       a (inc x)]
                   (rcf/! (+ a (inc x))))))
  % := 2
  (swap! !x inc)
  % := 4
  (swap! !x inc)
  % := 6
  (dispose))

; Since everything is a flow in Photon, it feels like nothing is, so you don't see the types
; (except at the missionary/photon interop boundary).

(tests
  "Approximately equivalent program as missionary, for understanding"
  (def !x (atom 0))
  (defn user []
    (let [<x (m/signal! (m/watch !x))
          <a (m/signal! (m/latest inc <x))]
      (m/latest rcf/! (m/latest + <a (m/latest inc <x)))))
  (def main (m/reactor (m/stream! (user))))                 ; entrypoint that runs for effect
  (def dispose (main (fn [_] (rcf/! ::success))             ; start process, will run until disposed
                     (fn [err] (rcf/! ::error) (rcf/! err))))
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
