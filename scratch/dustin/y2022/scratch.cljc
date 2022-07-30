(ns dustin.y2022.scratch
  (:require [missionary.core :as m]
            [clojure.core.async :as a]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :as rcf :refer [tests ! % with]])
  (:import [hyperfiddle.photon Pending Failure]
           (missionary Cancelled)))

(hyperfiddle.rcf/enable!)
