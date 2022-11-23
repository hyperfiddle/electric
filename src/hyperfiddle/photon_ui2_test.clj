(ns hyperfiddle.photon-ui2-test
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]))
(defmacro run [& body] `(with (p/run (binding [dom/node (dom/by-id "root")] ~@body))))
