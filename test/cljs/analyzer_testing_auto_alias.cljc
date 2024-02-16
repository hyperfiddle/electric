(ns cljs.analyzer-testing-auto-alias)

(defmacro auto-aliased [x] `(def ~x))
