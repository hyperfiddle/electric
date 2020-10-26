(ns dustin.import-vars)


(defmacro import-vars
  "Import and shadow all vars from the given namespace"
  [namespace]
  `(do
     ~@(mapcat
         (fn [[name#]]
           `[(def ~name# ~(symbol (str namespace) (str name#)))
             (alter-meta! (var ~name#) (constantly (meta (var ~(symbol (str namespace) (str name#))))))])
         `~(ns-publics namespace))))
