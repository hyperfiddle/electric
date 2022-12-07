(ns peter.y2022.do-in-order
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.rcf :as rcf :refer [% tap tests with]]
   [missionary.core :as m])
  (:import
   [hyperfiddle.photon Pending]))

(defmacro do-in-order [& body]
  (let [[fst & more] body]
    (if (seq more)
      `(when (do ~fst :done) (do-in-order ~@more))
      fst)))

(tests
  "do races"
  (with (p/run
          (try
            (tap (new (p/task->cp (m/sleep 1 :first))))
            (tap :second)
            (catch Pending _)))
    [% %] := [:second :first])
  "`do-in-order` makes them run in order"
  (with (p/run
          (try
            (do-in-order
              (tap (new (p/task->cp (m/sleep 1 :first))))
              (tap :second))
            (catch Pending _)))
    [% %] := [:first :second]))
