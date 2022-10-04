(ns contrib.missionary-contrib
  "staging area, to be considered for missionary inclusion?"
  (:require [clojure.core.async :as a]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]])
  (:import (missionary Cancelled)))

; Core.async interop

(defn poll-task "run task (or mbox) repeatedly, producing a stream of results"
  [task]
  (m/ap
    (loop [v (m/? task)]
      (m/amb v (recur (m/? task))))))
