(ns user.hello-world
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! %]]))

(tests
  "hello world"
  (def dispose (p/run (! "hello world")))
  % := "hello world"
  (dispose))
