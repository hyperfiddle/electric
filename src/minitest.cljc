(ns minitest
  (:require [hyperfiddle.rcf :as rcf])
  #?(:cljs (:require-macros [hyperfiddle.rcf])))

(defmacro tests [& body]
  `(rcf/tests ~@body))
