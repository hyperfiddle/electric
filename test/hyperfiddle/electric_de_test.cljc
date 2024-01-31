(ns hyperfiddle.electric-de-test
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap with %]]
            [hyperfiddle.electric-local-def-de :as l]))

(tests "hello world"
  (with ((l/single {} (tap "hello world")) tap tap)
    % := "hello world"))
