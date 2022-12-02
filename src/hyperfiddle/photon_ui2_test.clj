(ns hyperfiddle.photon-ui2-test
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :as rcf :refer [tap]]))
(defmacro setup [control] `(p/run (binding [dom/node (dom/by-id "root")] (tap (~@control (reset! it dom/node))))))
