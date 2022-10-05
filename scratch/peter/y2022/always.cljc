(ns peter.y2022.always
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]))

(defmacro always [& body] `(fn [& _#] (do ~@body)))

(tests
  (def f (always (inc 1)))
  (f 1) := 2
  (f nil) := 2
  (f :foo) := 2
  (f) := 2
  )

(comment

  (always (-> js/window .-location))
  )