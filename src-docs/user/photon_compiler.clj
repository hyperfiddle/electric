(ns user.photon-compiler
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-impl.compiler :refer [analyze]]
            [hyperfiddle.photon-impl.runtime :as r :refer [emit]]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))

; I'm also not sure what can p/fn and p/defn take as arguments, must they be continuous flows?

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

(tests
  "Equivalent dataflow diamond as missionary"
  (def !x (atom 0))
  (defn user []
    (let [<x (m/signal! (m/watch !x))
          <a (m/signal! (m/latest inc <x))]
      (m/latest rcf/! (m/latest + <a (m/latest inc <x)))))
  (def main (m/reactor (m/stream! (user))))                 ; entrypoint that runs for effect
  (def dispose (main (fn [v] (rcf/! [::success v]))         ; start process, will run until disposed
                     (fn [err] (rcf/! [::failure err]))))
  % := 2
  (swap! !x inc)
  % := 4
  (swap! !x inc)
  % := 6
  (dispose))
