(ns hyperfiddle.electric.impl.compiler)

;; edit me with my twin .clj variant
(defn extract-closure [o nargs]
  (if-some [as (:arities o)]
    (or (get as nargs) (get as :varargs)
      (throw (ex-info (str "You called " (or (:name o) "<unnamed-efn>") " with " nargs
                        " argument" (when (> nargs 1) "s") " but it only supports " (vec (keys as)))
               {})))
    o))
