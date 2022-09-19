(ns dustin.y2022.missionary-cancellation
  (:require [clojure.datafy :refer [datafy]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-impl.compiler :refer [analyze]]
            [hyperfiddle.photon-impl.runtime :as r :refer [emit]]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m])
  (:import (missionary Cancelled)))

(hyperfiddle.rcf/enable!)

(tests
  ""
  ; this signal is infinite,

  ; Why does watch throw on cancel? (L: It used to do that)
  ; consider:

  (def !x (atom 1))
  (def dispose (p/run (tap (let [x (new (m/watch !x))]
                           (if (pos? x)
                             (/ 1 x)                        ; guard div by zero
                             x)))))                         ; define it at zero
  ; L: it's reasonable to expect this code to not throw an exception in any case
  % := 1
  (swap! !x dec)

  ; let x creates a signal
  ; (/ 1 x) has a subscription to signal x
  ; cancel this subscription (subscriptions is how you listen to a signal) (signals internally have a list of callbacks, its same idea as a watch)
  ; the if switches
  ; the if needs to cancel the subscription (/ 1 x)
  ; however there is a transfer pending, so on cancellation of (/ 1 x) we must not transfer 0 to this subscription

  ; Where is the error?
  ; the conditional branch cancels the division branch which makes it fail, bc exceptions are not caught
  ; then the conditional signal sees the exception, but
  ; Photon ignores cancellation errors on the signals it creates

  ; Q: why does the watch cancellation bubble up?
  ; because that's the default behavior of uncaught errors in the reactor
  % := 0
  ; this branch has a reference to a signal that is ready to transfer zero
  ; How do you gracefully discard the div branch? bc
  ; when you cancel the signal you want to invalidate the zero

  (dispose)

  ; You don't want to run the division on an odd number
  ; if the watch switches to a state where it's going to transfer a node value and at this point you cancel it
  ; you cannot produce the odd value.
  ; after cancellation, we invalidate any pending value.

  )

(tests
  "missionary program with if on watch"
  (def !x (atom 1))

  (defn user []
    (let [<x  (m/signal! (m/watch !x))
          ;(if (pos? x) (/ 1 x) x)
          <<x (m/latest {true  (m/latest / (m/cp 1) <x)
                         false <x} (m/latest pos? <x))]
      (m/latest rcf/tap
                (m/cp (try (m/?< (m/?< <<x))
                           (catch Cancelled e (tap ::cancelled)))))))

  (def main (m/reactor (m/stream! (user))))
  (def dispose (main (fn [v] (rcf/tap [::success v]))
                     (fn [err] (rcf/tap [::failure err]))))
  % := 1
  (swap! !x dec)
  ; (m/latest / (m/cp 1) <x) is cancelled by m/cp due to <<x becoming ready
  % := ::cancelled
  % := 0
  (dispose)
  % := ::cancelled
  % := ::cancelled
  % := ::cancelled
  % := [::failure _]                                        ; watch cancelled
  )
