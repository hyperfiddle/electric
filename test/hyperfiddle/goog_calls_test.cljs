(ns hyperfiddle.goog-calls-test
  (:require [hyperfiddle.electric :as e]
            [goog.color]
            [goog.math :as gm])
  (:import [goog Uri]
           [goog.events EventType]))

(e/defn Main []
  (list
    (goog.color/hslToHex 0.5 0.5 0.5)
    (Uri. "http://example.com")
    EventType.CLICK
    goog.events.EventType.CLICK
    (gm/clamp -1 0 5)))
