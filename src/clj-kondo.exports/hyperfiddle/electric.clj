(ns hyperfiddle.electric)

(defmacro for-by
  [kf bindings & body]
  `(do ~kf
       (for ~bindings ~@body)))
