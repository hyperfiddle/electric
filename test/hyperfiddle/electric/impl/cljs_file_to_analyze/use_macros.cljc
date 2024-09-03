(ns hyperfiddle.electric.impl.cljs-file-to-analyze.use-macros)

(defmacro useme-mac [x] `(def ~x))
(defmacro renameme-mac [x] `(def ~x))
