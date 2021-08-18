(ns hyperfiddle.client.examples.clock
  (:require [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom]))

#?(:cljs (p/run (dom/set-text-content! (dom/by-id "clock") dom/clock)))