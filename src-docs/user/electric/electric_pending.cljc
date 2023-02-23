(ns user.electric.electric-pending
  (:require [hyperfiddle.electric :as p]
            [hyperfiddle.rcf :refer [tests tap % with]])
  (:import (hyperfiddle.electric Pending)))


(hyperfiddle.rcf/enable!)

(tests
  "Pending network transfer is trapped locally with reactive try/catch"
  (with (p/run (tap (try [(tap 1)
                          (tap (p/server 2))]
                         (catch Pending _
                           ::pending))))
    % := 1
    % := ::pending
    ; do not see 1 again
    % := 2
    % := [1 2]))
