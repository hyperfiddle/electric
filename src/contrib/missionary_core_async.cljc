(ns ^:deprecated ^:no-doc contrib.missionary-core-async
  "Deprecated: moved to dustingetz.missionary-core-async"
  (:require [dustingetz.missionary-core-async]))

(def chan-read! dustingetz.missionary-core-async/chan-read!)
(def chan->ap dustingetz.missionary-core-async/chan->ap)
(defmacro use-channel [& args] `(dustingetz.missionary-core-async/use-channel ~@args))
(def chan->task dustingetz.missionary-core-async/chan->task)
