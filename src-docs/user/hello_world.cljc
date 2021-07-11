(ns user.hello-world
  (:require [hfdl.lang :as r :refer [defnode]]
            [hyperfiddle.rcf :refer [tests ! %]]))

(tests
  "hello world"
  (def dispose (r/run (! "hello world")))
  % := "hello world"
  (dispose))

(comment
  "distributed hello world"
  (defn f [] #?(:cljs "hello" :clj "world"))
  (def dispose (r/run (! [(f) ~@(f)])))
  % := ["hello" "world"]
  (dispose))
