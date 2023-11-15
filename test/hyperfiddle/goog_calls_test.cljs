(ns hyperfiddle.goog-calls-test
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-local-def :as l]
            [goog.color]
            [goog.math :as gm]
            [goog.string.format]
            [goog.string :refer (format)])
  (:import [goog Uri]
           [goog.events EventType]))

(l/defn Main []
  (list
    (goog.color/hslToHex 0.5 0.5 0.5)
    (Uri. "http://example.com")
    EventType.CLICK
    goog.events.EventType.CLICK
    (gm/clamp -1 0 5)
    (format "%4d" 12)
    (js/matchMedia (e/watch (atom "(max-width: 600px)")))))
