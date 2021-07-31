(ns user.hello-world
  (:require [hfdl.lang :as p]
            [hyperfiddle.rcf :refer [tests ! %]]))

(tests
  "hello world"
  (def dispose (p/run (! "hello world")))
  % := "hello world"
  (dispose))

(comment
  "distributed hello world"
  (defn f [] #?(:cljs "hello" :clj "world"))
  (def dispose (p/run (! [(f) ~@(f)])))
  % := ["hello" "world"]
  (dispose))
