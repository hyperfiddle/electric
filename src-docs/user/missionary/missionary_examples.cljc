(ns user.missionary.missionary-examples
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

(tests
  "sliding window"
  (def >window
    (m/ap
      (let [>x (m/seed [1 2 3])]
        (m/zip vector (m/eduction (drop 1) >x) >x))))

  (def it (>window #(! ::notify) #(! ::terminate)))
  % := ::notify
  @it := [2 1]
  % := ::notify
  @it := [3 2]
  % := ::terminate
  (it))