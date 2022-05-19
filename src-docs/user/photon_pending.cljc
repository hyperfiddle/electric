(ns user.photon-pending
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]])
  (:import (hyperfiddle.photon Pending)))


(hyperfiddle.rcf/enable!)

(tests
  "Pending network transfer is trapped locally with reactive try/catch"
  (with (p/run (! (try [(! 1)
                        (! ~@2)]
                       (catch Pending _
                         ::pending))))
    % := 1
    % := ::pending
    ; do not see 1 again
    % := 2
    % := [1 2]))
