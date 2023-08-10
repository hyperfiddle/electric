(ns contrib.electric-contrib
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(defmacro after
  "2-arg: throw Pending for `ms`, then return `form`
  3-arg: return `init` for `ms`, then return `form`"
  ([ms form] `(case (new (e/task->cp (m/sleep ~ms))) ~form))
  ([ms init form] `(case (new (e/task->cp (m/sleep ~ms ::done) nil)) ::done ~form ~init)))

(defmacro keep-for [ms & body] `(when (new (e/task->cp (m/sleep ~ms false) true)) ~@body))

(comment
  (keep-for 1000 (dom/>>style :border-color "green")))

(defmacro always "like constantly, but runs the body every time you call it"
  [& body]
  (let [self (gensym)]
    `(fn ~self
       ([] (do ~@body))
       ([a#] (~self))
       ([a# b#] (~self))
       ([a# b# & more#] (~self)))))

(comment
  (constantly (rand-int)) vs (always (rand-int))
  (fn [_] (rand-int)) vs (always (rand-int)))
