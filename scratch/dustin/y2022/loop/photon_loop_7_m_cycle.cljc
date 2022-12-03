(ns dustin.y2022.loop.photon-loop-7-m-cycle
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.y2022.loop.photon-loop-7-m-cycle)))

(rcf/enable!)

(tests
  "We know how to cycle Photon circuit using assignment in Photon userland"
  (with (p/run (let [!X (atom (p/fn [] 0)) X (p/watch !X)]
                 (let [x (inc (tap (new X)))]
                   (reset! !X (p/fn [] x)))))
    % := 0
    % := 1 ; ❌
    % := ::rcf/timeout)) ; stabilize - work skipping

(tests
  "We also know how to cycle a missionary circuit using assignment in missionary userland"
  ; attempt 1:
  (def !main (m/reactor
               (m/stream!
                 (let [!X (atom (m/cp 0))
                       <<x (m/signal! (m/watch !X))
                       <x (m/signal! (m/cp (tap (m/?< (m/?< <<x)))))]
                   (m/latest inc <x (m/latest (partial reset! !X) (m/cp <x)))))))
  (with (!main #(tap [::success %]) #(tap [::failure %]))
    % := 0
    % := 1 ; ❌
    % := [::failure _])) ; Subscription cancelled (Dustin: I don't see why?)

(tests
  "Can we cycle a missionary circuit using unsupervised assignment?"

  )
