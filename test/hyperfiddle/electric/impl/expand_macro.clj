(ns hyperfiddle.electric.impl.expand-macro)

(defmacro twice [x] `[~x ~x])
