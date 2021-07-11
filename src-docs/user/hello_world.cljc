(ns user.hello-world
  (:require [hfdl.lang :as r :refer [defnode]]
            [hyperfiddle.rcf :refer [tests ! %]]))

(tests
  "hello world"
  (def dispose (r/run (! "hello world")))
  % := "hello world"
  (dispose))
