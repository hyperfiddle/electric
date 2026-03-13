(ns ^:deprecated contrib.missionary-contrib
  "Deprecated: moved to dustingetz.missionary-contrib"
  (:require [dustingetz.missionary-contrib]))

(def mix dustingetz.missionary-contrib/mix)
#?(:clj (def iterator-consumer dustingetz.missionary-contrib/iterator-consumer))
#?(:clj (def seq-consumer dustingetz.missionary-contrib/seq-consumer))
(def poll-task dustingetz.missionary-contrib/poll-task)
(def document dustingetz.missionary-contrib/document)
(def throttle dustingetz.missionary-contrib/throttle)
(def delay-flow dustingetz.missionary-contrib/delay-flow)
(def all dustingetz.missionary-contrib/all)
