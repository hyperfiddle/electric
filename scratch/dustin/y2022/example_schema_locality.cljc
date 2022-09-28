(ns dustin.y2022.example-schema-locality
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros dustin.y2022.example-schema-locality)))

(p/def schema)

(p/defn App [db]
  (p/server
    (binding [schema (new (p/task->cp (schema! db)))]
      (p/client
        (p/server
          (p/for [a (keys schema)]
            (p/client
              (dom/dl
                (dom/dt (pr-str a))
                (dom/dd (pr-str (p/server (a schema))))))))))))