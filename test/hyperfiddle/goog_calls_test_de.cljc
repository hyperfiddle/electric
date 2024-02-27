(ns hyperfiddle.goog-calls-test-de
  (:require [hyperfiddle.electric-de :as e]
            [hyperfiddle.electric.impl.lang-de2 :as lang]
            #?(:cljs [goog.color])
            #?(:cljs [goog.math :as gm])
            #?(:cljs [goog.string.format])
            #?(:cljs [goog.string :refer (format)]))
  #?(:cljs (:import [goog Uri]
                    [goog.events EventType])))

(e/defn Main []
  (e/client
    (list
      (goog.color/hslToHex 0.5 0.5 0.5)
      (Uri. "http://example.com")
      EventType.CLICK
      goog.events.EventType.CLICK
      (gm/clamp -1 0 5)
      (format "%4d" 12)
      (js/matchMedia (e/watch (atom "(max-width: 600px)"))))))
